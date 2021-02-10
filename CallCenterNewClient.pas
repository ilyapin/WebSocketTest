unit CallCenterNewClient;

interface

uses System.Classes, System.Sysutils, System.Generics.Collections, System.Generics.Defaults,
     System.SyncObjs, WinApi.Windows,
     superobject, HTTPWebsocketClient;

type
{
    FEmployeeOID: Int64;
    FPhone: string;
    property EmployeeOID : Int64 read FEmployeeOID write FEmployeeOID;
    property Phone : string read FPhone write FPhone;

}
  TCCNNotifyEvent = procedure (Sender:TObject; Obj:ISuperObject) of object;
  TCCNResponseEvent = procedure (Sender:TObject; Request:ISuperObject; Response:ISuperObject) of object;

  TCCNClient = class
  private
    FThreadStarted : Boolean;
    FThreadComplete : TEvent;
    FTerminated : Boolean;
    FSendQueue : TQueue<string>;
    FSendQueueEvent : TEvent;

    FClient : THTTPWebsocketClient;
    FUrl : string;

    FRequestTimeout: Cardinal;
    FWriteTimeout: Integer;
    FReadTimeout: Integer;
    FPingTimeout: Cardinal;
    FConnectTimeout: Integer;

    FOnResponse: TCCNResponseEvent;
    FOnNotification: TCCNNotifyEvent;
    FOnRequestTimedOut: TCCNNotifyEvent;
    FOnConnect: TNotifyEvent;

    procedure ThreadProc;
    procedure ThreadInitialize;
    procedure ThreadDoOnTextData(Sender : TObject; const Data: UTF8String);
    procedure ThreadCheckRequestTimeout;
    procedure ThreadFinalize;
    procedure ThreadDoOnConnect;

    procedure SendQueueLock;
    procedure SendQueueUnlock;
    procedure RequestListLock;
    procedure RequestListUnlock;
  protected
    type
      TRequestRec = record
        Request : ISuperObject;
        Ticks : Cardinal;
      end;
  protected
    FRequestID : Integer;
    FRequestsSent : TDictionary<Integer, TRequestRec>;
    procedure QueueSend(const Data:string);
  public
    property Url : string read FUrl write FUrl;
    property RequestTimeout : Cardinal read FRequestTimeout write FRequestTimeout;
    property WriteTimeout: Integer read FWriteTimeout write FWriteTimeout;
    property ReadTimeout: Integer read FReadTimeout write FReadTimeout;
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout;
    property PingTimeout: Cardinal read FPingTimeout write FPingTimeout;

    constructor Create;
    destructor Destroy; override;

    property IsRunning : Boolean read FThreadStarted;
    procedure Run;
    procedure Stop;

    procedure SendRequest(const Data:ISuperObject);
    procedure SendNotification(const Data:ISuperObject);

    property OnConnect : TNotifyEvent read FOnConnect write FOnConnect;
    property OnResponse : TCCNResponseEvent read FOnResponse write FOnResponse;
    property OnNotification : TCCNNotifyEvent read FOnNotification write FOnNotification;
    property OnRequestTimedOut : TCCNNotifyEvent read FOnRequestTimedOut write FOnRequestTimedOut;
  end;

implementation

uses IdURI, IdSSLOpenSSL;

{ TCCNWebSocketClient}

constructor TCCNClient.Create;
begin
  inherited Create;

  FRequestID := 0;
  FThreadComplete := TEvent.Create(nil, True, False, '');
  FThreadStarted := False;
  FUrl := Url;

  FRequestTimeout := 10000;
  FWriteTimeout := 2000;
  FReadTimeout := 5000;
  FConnectTimeout := 5000;
  FPingTimeout := 10000;

  FSendQueue := TQueue<string>.Create;
  FSendQueueEvent := TEvent.Create(nil, True, False, '');
  FRequestsSent := TDictionary<Integer, TRequestRec>.Create;
end;

destructor TCCNClient.Destroy;
begin
  Stop;
  FreeAndNil(FSendQueue);
  FreeAndNil(FRequestsSent);
  FreeAndNil(FThreadComplete);
  FreeAndNil(FSendQueueEvent);
  inherited;
end;

procedure TCCNClient.RequestListLock;
begin
  TMonitor.Enter(FRequestsSent);
end;

procedure TCCNClient.RequestListUnlock;
begin
  TMonitor.Exit(FRequestsSent);
end;

procedure TCCNClient.Run;
begin
  if not FThreadStarted then
    TThread.CreateAnonymousThread(ThreadProc).Start;
  FThreadStarted := True;
end;

procedure TCCNClient.QueueSend(const Data: string);
begin
  SendQueueLock;
  try
    FSendQueue.Enqueue(Data);
  finally
    SendQueueUnlock;
  end;
  FSendQueueEvent.SetEvent;
end;

procedure TCCNClient.SendNotification(const Data: ISuperObject);
begin
  QueueSend(Data.AsJSon);
end;

procedure TCCNClient.SendQueueLock;
begin
  TMonitor.Enter(FSendQueue);
end;

procedure TCCNClient.SendQueueUnlock;
begin
  TMonitor.Exit(FSendQueue);
end;

procedure TCCNClient.SendRequest(const Data: ISuperObject);
var
  RequestRec : TRequestRec;
begin
  if Assigned(Data) then
  begin
    RequestListLock;
    try
      inc(FRequestID);
      RequestRec.Request := Data;
      RequestRec.Ticks := GetTickCount;
      RequestRec.Request.I['RequestId'] := FRequestID;
      FRequestsSent.Add(FRequestID, RequestRec);
    finally
      RequestListUnlock;
    end;
    QueueSend(Data.AsJSon);
  end;
end;

procedure TCCNClient.Stop;
begin
  if FThreadStarted then
  begin
    FTerminated := True;
    FSendQueueEvent.SetEvent;
    FThreadComplete.WaitFor;
    FThreadStarted := False;
  end;
end;

procedure TCCNClient.ThreadInitialize;
begin
  FClient := THTTPWebsocketClient.Create;
  FClient.WSUrl := Url;
  FClient.OnTextData := ThreadDoOnTextData;
  FClient.IOHandler.SSLOptions.Mode := sslmClient;
  FClient.IOHandler.SSLOptions.Method := sslvSSLv23;
  FClient.WriteTimeout := WriteTimeout;
  FClient.IOHandler.ReadTimeout := ReadTimeout;
end;

procedure TCCNClient.ThreadFinalize;
begin
  if FClient.Connected then
    FClient.Disconnect;
  FreeAndNil(FClient);
end;

procedure TCCNClient.ThreadCheckRequestTimeout;
var
  Item : TPair<Integer,TRequestRec>;
  CurrentTicks : Cardinal;
  KeysToDelete : array of Integer;
  RequestsTimedOut : array of ISuperObject;
  Key : Integer;
  Request : ISuperObject;

  procedure Add(const Pair:TPair<Integer,TRequestRec>);
  var
    len : Integer;
  begin
    len := Length(KeysToDelete);
    SetLength(KeysToDelete, len + 1);
    KeysToDelete[len] := Pair.Key;
    SetLength(RequestsTimedOut, len + 1);
    RequestsTimedOut[len] := Pair.Value.Request;
  end;
begin
  CurrentTicks := GetTickCount;
  RequestListLock;
  try
    for Item in FRequestsSent do
    begin
      if (CurrentTicks - Item.Value.Ticks) >= RequestTimeout then
        Add(Item);
    end;
    if Length(KeysToDelete) > 0 then
    begin
      for Key in KeysToDelete do
        FRequestsSent.Remove(Key);
    end;
  finally
    RequestListUnlock;
  end;

  if Assigned(OnRequestTimedOut) and (Length(RequestsTimedOut) > 0) then
  begin
    for Request in RequestsTimedOut do
    begin
      TThread.Synchronize(nil, procedure
                               begin
                                 OnRequestTimedOut(self, Request)
                               end);
    end;
  end;
end;

procedure TCCNClient.ThreadDoOnConnect;
begin
  if Assigned(FOnConnect) then
      TThread.Synchronize(nil, procedure
                               begin
                                 OnConnect(self);
                               end);
end;

procedure TCCNClient.ThreadDoOnTextData(Sender : TObject; const Data: UTF8String);
var
  RequestRec : TRequestRec;
  Response, Request : ISuperObject;
  RequestId : Integer;
begin
  Request := nil;
  Response := TSuperObject.ParseString(PChar(UTF8ToString(Data)), true);
  if Assigned(Response) then
  begin
    RequestId := Response.I['RequestId'];
    if RequestId > 0 then
    begin
      RequestListLock;
      try
        if FRequestsSent.TryGetValue(RequestId, RequestRec) then
        begin
          Request := RequestRec.Request;
          FRequestsSent.Remove(RequestId);
        end;
      finally
        RequestListUnlock;
      end;
    end;

    if Assigned(Request) then
    begin
      if Assigned(FOnResponse) then
        TThread.Synchronize(nil, procedure
                                 begin
                                   OnResponse(self, Request, Response)
                                 end);
    end
    else if Assigned(FOnNotification) then
    begin
      TThread.Synchronize(nil, procedure
                               begin
                                 OnNotification(self, Response)
                               end);
    end;
  end;
end;

procedure TCCNClient.ThreadProc;
var
  Data2Send : TArray<string>;
  i : integer;
begin
  try
    ThreadInitialize;
    try
      while not FTerminated do
      try
        if not FClient.Connected then
        begin
          FClient.Connect;
          FClient.UpgradeToWebsocket;
          ThreadDoOnConnect;
        end;
        if wrSignaled = FSendQueueEvent.WaitFor(300) then
        begin
          SendQueueLock;
          try
            Data2Send := FSendQueue.ToArray;
            FSendQueue.Clear;
            FSendQueueEvent.ResetEvent;
          finally
            SendQueueUnlock;
          end;
          // send
          for i := 0 to Length(Data2Send) - 1 do
            FClient.IOHandler.Write(Data2Send[i]);
        end;
        // recieve
        FClient.ReadAndProcessData;
        // check timeout
        ThreadCheckRequestTimeout;
        // ping
        if (GetTickCount - FClient.IOHandler.LastActivityTicks) > PingTimeout then
          FClient.Ping;
      except
        FClient.Disconnect;
        //?? todo log
      end;
    finally
      ThreadFinalize;
    end;
  finally
    FThreadComplete.SetEvent;
  end;
end;

end.
