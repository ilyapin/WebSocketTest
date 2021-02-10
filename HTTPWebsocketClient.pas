unit HTTPWebsocketClient;

interface

uses
  Classes,
  WinApi.Windows,
  IdHTTP,
  Types,
  IdHashSHA,
  IdIOHandler,
  IdException,
  IdBuffer,
  IdGlobal,
  Idwinsock2,
  IdSSLOpenSSL;

type
  TWSDataType      = (wdtText, wdtBinary);
  TWSDataCode      = (wdcNone, wdcContinuation, wdcText, wdcBinary, wdcClose, wdcPing, wdcPong);
  TWSExtensionBit  = (webBit1, webBit2, webBit3);
  TWSExtensionBits = set of TWSExtensionBit;

  TWebsocketIOHandler   = class;
  EIdWebSocketHandleError = class(EIdSocketHandleError);

  TIOWSPayloadInfo = Record
    aPayloadLength: Cardinal;
    aDataCode: TWSDataCode;
    procedure Initialize(iTextMode:Boolean; iPayloadLength:Cardinal);
    function DecLength(value:Cardinal):boolean;
    procedure Clear;
  end;

  TWebsocketIOHandler = class(TIdSSLIOHandlerSocketOpenSSL)
  private
    FIsWebsocket: Boolean;
    FWSInputBuffer: TIdBuffer;
    FLastActivityTicks: Cardinal;
    procedure SetIsWebsocket(const Value: Boolean);
  protected
    FMessageStream: TMemoryStream;
    FCloseCodeSend: Boolean;
    FPayloadInfo: TIOWSPayloadInfo;

    function InternalReadDataFromSource(var VBuffer: TIdBytes): Integer;
    function ReadDataFromSource(var VBuffer: TIdBytes): Integer; override;
    function WriteDataToTarget (const ABuffer: TIdBytes; const AOffset, ALength: Integer): Integer; override;

    function ReadFrame(out aFIN, aRSV1, aRSV2, aRSV3: boolean; out aDataCode: TWSDataCode; out aData: TIdBytes): Integer;
    function ReadMessage(var aBuffer: TIdBytes; out aDataCode: TWSDataCode): Integer;

    function UTF8Encoding: IIdTextEncoding;
    procedure InitComponent; override;
  public
    destructor Destroy; override;

    function WriteData(aData: TIdBytes; aType: TWSDataCode;
                        aFIN: boolean = true; aRSV1: boolean = false; aRSV2: boolean = false; aRSV3: boolean = false): integer;
    property IsWebsocket   : Boolean read FIsWebsocket   write SetIsWebsocket;

    function  HasData: Boolean;
    procedure Clear;
    function  Readable(AMSec: Integer = IdTimeoutDefault): Boolean; override;

    procedure Close; override;

    procedure Write(const AOut: string; AEncoding: IIdTextEncoding = nil); overload; override;
    procedure WriteLn(const AOut: string; AEncoding: IIdTextEncoding = nil); overload; override;
    procedure WriteLnRFC(const AOut: string = ''; AEncoding: IIdTextEncoding = nil); override;
    procedure Write(AValue: TStrings; AWriteLinesCount: Boolean = False; AEncoding: IIdTextEncoding = nil); overload; override;
    procedure Write(AStream: TStream; aType: TWSDataType); overload;
    procedure WriteBufferFlush(AByteCount: Integer); override;

    property  LastActivityTicks: Cardinal read FLastActivityTicks write FLastActivityTicks;
  end;

  TWebsocketMsgText = procedure(Sender : TObject; const Data: UTF8String) of object;

  THTTPWebsocketClient = class(TIdHTTP)
  private
    FHash: TIdHashSHA1;
    FOnTextData : TWebsocketMsgText;
    FWriteTimeout: Integer;
    FUrl: string;
    function  GetIOHandlerWS: TWebsocketIOHandler;
    procedure SetIOHandlerWS(const Value: TWebsocketIOHandler);
    procedure SetWriteTimeout(const Value: Integer);
    procedure SetUrl(const Value: string);
  protected
    function HttpUrl : string;
    procedure DoOnTextData(const Data: UTF8String);virtual;
    procedure InternalUpgradeToWebsocket;virtual;
    function  MakeImplicitClientHandler: TIdIOHandler; override;
  public
    procedure  AfterConstruction; override;
    destructor Destroy; override;

    procedure UpgradeToWebsocket;
    procedure Connect; override;
    procedure Disconnect(ANotifyPeer: Boolean); override;
    function  CheckConnection: Boolean;
    procedure Ping;
    procedure ReadAndProcessData;

    property  IOHandler: TWebsocketIOHandler read GetIOHandlerWS write SetIOHandlerWS;
  published
    property  Host;
    property  Port;
    property  WSUrl: string read FUrl write SetUrl;

    property  WriteTimeout: Integer read FWriteTimeout write SetWriteTimeout;
    property  OnTextData: TWebsocketMsgText read FOnTextData write FOnTextData;
  end;

function RandomStr(len:integer):string;

implementation

uses
  IdCoderMIME,
  System.SysUtils,
  System.Math,
  IdURI,
  IdResourceStringsCore,
  IdExceptionCore,
  IdResourceStrings,
  IdStack,
  IdStream;

function RandomStr(len:integer):string;
const
	chars : string = 'abcdefghijklmnopqrstuvwxyz1234567890';
var
  i, j : integer;
begin
  SetLength(Result, len);
  for i := 1 to len do
  begin
    j := Random(Length(chars)) + 1;
    Result[i] := chars[j];
  end;
end;

{ TIOWSPayloadInfo }

procedure TIOWSPayloadInfo.Initialize(iTextMode:Boolean; iPayloadLength:Cardinal);
begin
  aPayloadLength := iPayloadLength;
  if iTextMode then
    aDataCode := wdcText
  else
    aDataCode := wdcBinary;
end;

procedure TIOWSPayloadInfo.Clear;
begin
  aPayloadLength := 0;
  aDataCode := wdcBinary;
end;

function TIOWSPayloadInfo.DecLength(Value:Cardinal):boolean;
begin
  if aPayloadLength >= value then
    aPayloadLength := aPayloadLength - value
  else
    aPayloadLength := 0;
  aDataCode := wdcContinuation;
  Result := aPayloadLength = 0;
end;

{ TIdIOHandlerWebsocket }
//close frame codes
const
  C_FrameClose_Normal            = 1000; //1000 indicates a normal closure, meaning that the purpose for
                                         //which the connection was established has been fulfilled.
  C_FrameClose_GoingAway         = 1001; //1001 indicates that an endpoint is "going away", such as a server
                                         //going down or a browser having navigated away from a page.
  C_FrameClose_ProtocolError     = 1002; //1002 indicates that an endpoint is terminating the connection due
                                         //to a protocol error.
  C_FrameClose_UnhandledDataType = 1003; //1003 indicates that an endpoint is terminating the connection
                                         //because it has received a type of data it cannot accept (e.g., an
                                         //endpoint that understands only text data MAY send this if it
                                         //receives a binary message).
  C_FrameClose_Reserved          = 1004; //Reserved.  The specific meaning might be defined in the future.
  C_FrameClose_ReservedNoStatus  = 1005; //1005 is a reserved value and MUST NOT be set as a status code in a
                                         //Close control frame by an endpoint.  It is designated for use in
                                         //applications expecting a status code to indicate that no status
                                         //code was actually present.
  C_FrameClose_ReservedAbnormal  = 1006; //1006 is a reserved value and MUST NOT be set as a status code in a
                                         //Close control frame by an endpoint.  It is designated for use in
                                         //applications expecting a status code to indicate that the
                                         //connection was closed abnormally, e.g., without sending or
                                         //receiving a Close control frame.
  C_FrameClose_InconsistentData  = 1007; //1007 indicates that an endpoint is terminating the connection
                                         //because it has received data within a message that was not
                                         //consistent with the type of the message (e.g., non-UTF-8 [RFC3629]
                                         //data within a text message).
  C_FrameClose_PolicyError       = 1008; //1008 indicates that an endpoint is terminating the connection
                                         //because it has received a message that violates its policy.  This
                                         //is a generic status code that can be returned when there is no
                                         //other more suitable status code (e.g., 1003 or 1009) or if there
                                         //is a need to hide specific details about the policy.
  C_FrameClose_ToBigMessage      = 1009; //1009 indicates that an endpoint is terminating the connection
                                         //because it has received a message that is too big for it to process.
  C_FrameClose_MissingExtenstion = 1010; //1010 indicates that an endpoint (client) is terminating the
                                         //connection because it has expected the server to negotiate one or
                                         //more extension, but the server didn't return them in the response
                                         //message of the WebSocket handshake.  The list of extensions that
                                         //are needed SHOULD appear in the /reason/ part of the Close frame.
                                         //Note that this status code is not used by the server, because it
                                         //can fail the WebSocket handshake instead.
  C_FrameClose_UnExpectedError   = 1011; //1011 indicates that a server is terminating the connection because
                                         //it encountered an unexpected condition that prevented it from
                                         //fulfilling the request.
  C_FrameClose_ReservedTLSError  = 1015; //1015 is a reserved value and MUST NOT be set as a status code in a
                                         //Close control frame by an endpoint.  It is designated for use in
                                         //applications expecting a status code to indicate that the
                                         //connection was closed due to a failure to perform a TLS handshake
                                         //(e.g., the server certificate can't be verified).

//frame codes
const
  C_FrameCode_Continuation = 0;
  C_FrameCode_Text         = 1;
  C_FrameCode_Binary       = 2;
  //3-7 are reserved for further non-control frames
  C_FrameCode_Close        = 8;
  C_FrameCode_Ping         = 9;
  C_FrameCode_Pong         = 10 {A};
  //B-F are reserved for further control frames


procedure TWebsocketIOHandler.InitComponent;
begin
  inherited ;
  FMessageStream := TMemoryStream.Create;
  FWSInputBuffer := TIdBuffer.Create;
end;

procedure TWebsocketIOHandler.Clear;
begin
  FWSInputBuffer.Clear;
  InputBuffer.Clear;
  FIsWebsocket := False;
  FLastActivityTicks := 0;
  FPayloadInfo.Clear;
  FCloseCodeSend := False;
end;

procedure TWebsocketIOHandler.Close;
begin
  IsWebsocket := False;
  inherited Close;
end;

destructor TWebsocketIOHandler.Destroy;
begin
  FWSInputBuffer.Free;
  FMessageStream.Free;
  inherited;
end;

function TWebsocketIOHandler.HasData: Boolean;
begin
  Result := (FWSInputBuffer.Size > 0) or not InputBufferIsEmpty;
end;

function TWebsocketIOHandler.InternalReadDataFromSource(var VBuffer: TIdBytes): Integer;
begin
  SetLength(VBuffer, 0);

  CheckForDisconnect;
  if not Readable(ReadTimeout) or
     not Opened or
     not SourceIsAvailable then
  begin
    CheckForDisconnect;
    if not Opened then
      raise EIdNotConnected.Create(RSNotConnected)
    else if not SourceIsAvailable then
      raise EIdClosedSocket.Create(RSStatusDisconnected);
    GStack.CheckForSocketError(GStack.WSGetLastError);

    raise EIdReadTimeout.Create(RSIdNoDataToRead);
  end;

  SetLength(VBuffer, RecvBufferSize);
  Result := inherited ReadDataFromSource(VBuffer);
  if Result = 0 then
  begin
    CheckForDisconnect;
    GStack.CheckForSocketError(GStack.WSGetLastError);

    raise EIdNoDataToRead.Create(RSIdNoDataToRead);
  end;
  SetLength(VBuffer, Result);
end;

procedure TWebsocketIOHandler.WriteLn(const AOut:string; AEncoding: IIdTextEncoding);
begin
  try
    FPayloadInfo.Initialize(True,0);
    inherited WriteLn(AOut, UTF8Encoding);
  finally
    FPayloadInfo.Clear;
  end;
end;

procedure TWebsocketIOHandler.WriteLnRFC(const AOut: string; AEncoding: IIdTextEncoding);
begin
  try
    FPayloadInfo.Initialize(True,0);
    inherited WriteLnRFC(AOut, UTF8Encoding);
  finally
    FPayloadInfo.Clear;
  end;
end;

procedure TWebsocketIOHandler.Write(const AOut: string; AEncoding: IIdTextEncoding);
begin
  try
    FPayloadInfo.Initialize(True,0);
    inherited Write(AOut, UTF8Encoding);
  finally
    FPayloadInfo.Clear;
  end;
end;

procedure TWebsocketIOHandler.Write(AValue: TStrings; AWriteLinesCount: Boolean; AEncoding: IIdTextEncoding);
begin
  try
    FPayloadInfo.Initialize(True,0);
    inherited Write(AValue, AWriteLinesCount, UTF8Encoding);
  finally
    FPayloadInfo.Clear;
  end;
end;

procedure TWebsocketIOHandler.Write(AStream: TStream; aType: TWSDataType);
begin
  try
    FPayloadInfo.Initialize((aType = wdtText),AStream.Size);
    inherited Write(AStream);
  finally
    FPayloadInfo.Clear;
  end;
end;

procedure TWebsocketIOHandler.WriteBufferFlush(AByteCount: Integer);
begin
  if Assigned(FWriteBuffer) and (FWriteBuffer.Size > 0) then
    inherited WriteBufferFlush(AByteCount);
end;

function TWebsocketIOHandler.WriteDataToTarget(const ABuffer: TIdBytes; const AOffset, ALength: Integer): Integer;
var
  data: TIdBytes;
  DataCode:TWSDataCode;
  fin:boolean;
begin
  if not IsWebsocket then
    Result := inherited WriteDataToTarget(ABuffer, AOffset, ALength)
  else
  begin
    data := ToBytes(ABuffer, ALength, AOffset);
    try
      DataCode := FPayloadInfo.aDataCode;
      fin := FPayloadInfo.DecLength(ALength);
      Result := WriteData(data, DataCode, fin, False, False, False);
    except
      FClosedGracefully := True;
      Raise;
    end;
  end;
end;

function TWebsocketIOHandler.Readable(AMSec: Integer): Boolean;
begin
  Result := FWSInputBuffer.Size > 0;
  if not Result then
    Result := inherited Readable(AMSec);
end;

function TWebsocketIOHandler.ReadDataFromSource(var VBuffer: TIdBytes): Integer;
var
  WSCode: TWSDataCode;
begin
  if not IsWebsocket then
    Result := inherited ReadDataFromSource(VBuffer)
  else
  begin
    try
      //we wait till we have a full message here (can be fragmented in several frames)
      Result := ReadMessage(VBuffer, WSCode);

      //first write the data code (text or binary, ping, pong)
      FInputBuffer.Write(LongWord(Ord(WSCode)));
      // we write message size here, vbuffer is written after this.
      // is way we can use ReadStream to get 1 single message
      // (in case multiple messages in FInputBuffer)
      if LargeStream then
        FInputBuffer.Write(Int64(Result))
      else
        FInputBuffer.Write(LongWord(Result))
    except
      FClosedGracefully := True;
      Raise;
    end;
  end;
end;

function TWebsocketIOHandler.ReadMessage(var aBuffer: TIdBytes; out aDataCode: TWSDataCode): Integer;
var
  iReadCount: Integer;
  iaReadBuffer: TIdBytes;
  bFIN, bRSV1, bRSV2, bRSV3: boolean;
  lDataCode: TWSDataCode;
  lFirstDataCode: TWSDataCode;
begin
  Result := 0;
//     ...all fragments of a message are of
//     the same type, as set by the first fragment's opcode.  Since
//     control frames cannot be fragmented, the type for all fragments in
//     a message MUST be either text, binary, or one of the reserved
//     opcodes.
  lFirstDataCode := wdcNone;
  FMessageStream.Clear;

  repeat
    //read a single frame
    iReadCount := ReadFrame(bFIN, bRSV1, bRSV2, bRSV3, lDataCode, iaReadBuffer);
    if (iReadCount > 0) or
       (lDataCode <> wdcNone) then
    begin
      Assert(Length(iaReadBuffer) = iReadCount);

      //process frame
      case lDataCode of
        wdcText, wdcBinary:
          begin
            if lFirstDataCode <> wdcNone then
              raise EIdWebSocketHandleError.Create('Invalid frame: specified data code only allowed for the first frame. Data = ' + BytesToStringRaw(iaReadBuffer));
            lFirstDataCode := lDataCode;

            FMessageStream.Clear;
            TIdStreamHelper.Write(FMessageStream, iaReadBuffer);
          end;
        wdcContinuation:
          begin
            if not (lFirstDataCode in [wdcText, wdcBinary]) then
              raise EIdWebSocketHandleError.Create('Invalid frame continuation. Data = ' + BytesToStringRaw(iaReadBuffer));
            TIdStreamHelper.Write(FMessageStream, iaReadBuffer);
          end;
        wdcClose:
          begin
            Close;
          end;
        //Note: control frames can be send between fragmented frames
        wdcPing:
        begin
          WriteData(iaReadBuffer, wdcPong);
          lFirstDataCode := lDataCode;
        end;
        wdcPong:
        begin
           //pong received, ignore;
          lFirstDataCode := lDataCode;
        end;
      end;
    end
    else
      Break;
  until bFIN;

  if bFIN then
  begin
    if (lFirstDataCode in [wdcText, wdcBinary]) then
    begin
      //result
      FMessageStream.Position := 0;
      TIdStreamHelper.ReadBytes(FMessageStream, aBuffer);
      Result    := FMessageStream.Size;
      aDataCode := lFirstDataCode
    end
    else if (lFirstDataCode in [wdcPing, wdcPong]) then
    begin
      //result
      FMessageStream.Position := 0;
      TIdStreamHelper.ReadBytes(FMessageStream, aBuffer);
      SetLength(aBuffer, FMessageStream.Size);
      //dummy data: there *must* be some data read otherwise connection is closed by Indy!
      if Length(aBuffer) <= 0 then
      begin
        SetLength(aBuffer, 1);
        aBuffer[0] := Ord(lFirstDataCode);
      end;

      Result    := Length(aBuffer);
      aDataCode := lFirstDataCode
    end;
  end;
end;

procedure TWebsocketIOHandler.SetIsWebsocket(const Value: Boolean);
var
  data: TIdBytes;
begin
  //copy websocket data which was send/received during http upgrade
  if not FIsWebsocket and Value and (FInputBuffer.Size > 0) then
  begin
    FInputBuffer.ExtractToBytes(data);
    FWSInputBuffer.Write(data);
  end;

  FIsWebsocket := Value;
end;

function TWebsocketIOHandler.UTF8Encoding: IIdTextEncoding;
begin
  Result := IndyTextEncoding_UTF8;
end;

function TWebsocketIOHandler.ReadFrame(out aFIN, aRSV1, aRSV2, aRSV3: boolean; out aDataCode: TWSDataCode; out aData: TIdBytes): Integer;
var
  iInputPos: NativeInt;

  function _WaitByte(ARaiseExceptionOnTimeout: Boolean): Boolean;
  var
    temp: TIdBytes;
  begin
    if (FWSInputBuffer.Size > iInputPos) then
      Exit(True);

    Result := InternalReadDataFromSource(temp) > 0;
    if Result then
      FWSInputBuffer.Write(temp);
  end;

  function _GetByte: Byte;
  begin
    while FWSInputBuffer.Size <= iInputPos do
    begin
      _WaitByte(True);
      if FWSInputBuffer.Size <= iInputPos then
        Sleep(1);
    end;

    Result := FWSInputBuffer.PeekByte(iInputPos);
    inc(iInputPos);
  end;

  function _GetBytes(aCount: Integer): TIdBytes;
  var
    temp: TIdBytes;
  begin
    while FWSInputBuffer.Size < aCount do
    begin
      InternalReadDataFromSource(temp);
      FWSInputBuffer.Write(temp);
      if FWSInputBuffer.Size < aCount then
        Sleep(1);
    end;

    FWSInputBuffer.ExtractToBytes(Result, aCount);
  end;

var
  iByte: Byte;
  i, iCode: NativeInt;
  bHasMask: boolean;
  iDataLength, iPos: Int64;
  rMask: record
    case Boolean of
      True : (MaskAsBytes: array[0..3] of Byte);
      False: (MaskAsInt  : Int32);
  end;
begin
  iInputPos := 0;
  Result := 0;
  aFIN := False; aRSV1 := False; aRSV2:= False; aRSV3:= False;
  aDataCode := wdcNone;
  SetLength(aData, 0);

  if not _WaitByte(False) then
    Exit;
  FLastActivityTicks := GetTickCount;

  //wait + process data
  iByte := _GetByte;
  (* 0                   1                   2                   3
     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 (nr)
     7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0 (bit)
    +-+-+-+-+-------+-+-------------+-------------------------------+
    |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
    |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
    |N|V|V|V|       |S|             |   (if payload len==126/127)   |
    | |1|2|3|       |K|             |                               |
    +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - + *)
  //FIN, RSV1, RSV2, RSV3: 1 bit each
  aFIN   := (iByte and (1 shl 7)) > 0;
  aRSV1  := (iByte and (1 shl 6)) > 0;
  aRSV2  := (iByte and (1 shl 5)) > 0;
  aRSV3  := (iByte and (1 shl 4)) > 0;
  //Opcode: 4 bits
  iCode  := (iByte and $0F); //clear 4 MSB's
  case iCode of
    C_FrameCode_Continuation: aDataCode := wdcContinuation;
    C_FrameCode_Text:         aDataCode := wdcText;
    C_FrameCode_Binary:       aDataCode := wdcBinary;
    C_FrameCode_Close:        aDataCode := wdcClose;
    C_FrameCode_Ping:         aDataCode := wdcPing;
    C_FrameCode_Pong:         aDataCode := wdcPong;
  else
    raise EIdException.CreateFmt('Неизвестный Opcode : %d', [iCode]);
  end;

  //Mask: 1 bit
  iByte       := _GetByte;
  bHasMask    := (iByte and (1 shl 7)) > 0;
  //Length (7 bits or 7+16 bits or 7+64 bits)
  iDataLength := (iByte and $7F);  //clear 1 MSB
  //Extended payload length?
  //If 126, the following 2 bytes interpreted as a 16-bit unsigned integer are the payload length
  if (iDataLength = 126) then
  begin
    iByte       := _GetByte;
    iDataLength := (iByte shl 8); //8 MSB
    iByte       := _GetByte;
    iDataLength := iDataLength + iByte;
  end
  //If 127, the following 8 bytes interpreted as a 64-bit unsigned integer (the most significant bit MUST be 0) are the payload length
  else if (iDataLength = 127) then
  begin
    iDataLength := 0;
    for i := 7 downto 0 do  //read 8 bytes in reverse order
    begin
      iByte       := _GetByte;
      iDataLength := iDataLength +
                     (Int64(iByte) shl (8 * i)); //shift bits to left to recreate 64bit integer
    end;
    Assert(iDataLength > 0);
  end;

  //"All frames sent from client to server must have this bit set to 1"
  if bHasMask then
    raise EIdWebSocketHandleError.Create('Mask supplied but mask is not allowed for servers when sending data to clients. Buffer = ' + FWSInputBuffer.AsString);

  //Masking-key: 0 or 4 bytes
  if bHasMask then
  begin
    rMask.MaskAsBytes[0] := _GetByte;
    rMask.MaskAsBytes[1] := _GetByte;
    rMask.MaskAsBytes[2] := _GetByte;
    rMask.MaskAsBytes[3] := _GetByte;
  end;
  //Payload data:  (x+y) bytes
  FWSInputBuffer.Remove(iInputPos);  //remove header
  //simple read?
  if not bHasMask then
    aData := _GetBytes(iDataLength)
  else
  //reverse mask
  begin
    aData := _GetBytes(iDataLength);
    iPos   := 0;
    while iPos < iDataLength do
    begin
      aData[iPos] := aData[iPos] xor
                      rMask.MaskAsBytes[iPos mod 4]; //apply mask
      inc(iPos);
    end;
  end;

  Result := Length(aData);
end;

function TWebsocketIOHandler.WriteData(aData:TIdBytes; aType:TWSDataCode; aFIN,aRSV1,aRSV2,aRSV3:boolean): integer;
var
  iByte: Byte;
  i, ioffset: NativeInt;
  iDataLength, iPos: Int64;
  rLength: Int64Rec;
  rMask: record
    case Boolean of
      True : (MaskAsBytes: array[0..3] of Byte);
      False: (MaskAsInt  : Int32);
  end;
  strmData: TMemoryStream;
  bData: TIdBytes;
begin
  Assert(Binding <> nil);

  strmData := TMemoryStream.Create;
  try
    FLastActivityTicks := GetTickCount;   //sending some data
    (* 0                   1                   2                   3
       0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 (nr)
       7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0 (bit)
      +-+-+-+-+-------+-+-------------+-------------------------------+
      |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
      |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
      |N|V|V|V|       |S|             |   (if payload len==126/127)   |
      | |1|2|3|       |K|             |                               |
      +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - + *)
    //FIN, RSV1, RSV2, RSV3: 1 bit each
    iByte := 0;
    if aFIN  then iByte := iByte or (1 shl 7);
    if aRSV1 then iByte := iByte or (1 shl 6);
    if aRSV2 then iByte := iByte or (1 shl 5);
    if aRSV3 then iByte := iByte or (1 shl 4);
    //Opcode: 4 bits
    case aType of
      wdcContinuation : iByte := iByte + C_FrameCode_Continuation;
      wdcText         : iByte := iByte + C_FrameCode_Text;
      wdcBinary       : iByte := iByte + C_FrameCode_Binary;
      wdcClose        : iByte := iByte + C_FrameCode_Close;
      wdcPing         : iByte := iByte + C_FrameCode_Ping;
      wdcPong         : iByte := iByte + C_FrameCode_Pong;
    else
      raise EIdException.CreateFmt('Unsupported Opcode: %d', [Ord(aType)]);
    end;
    strmData.Write(iByte, SizeOf(iByte));

    iByte := 0;
    //Mask: 1 bit; Note: Clients must apply a mask
    iByte := (1 shl 7);

    //Length: 7 bits or 7+16 bits or 7+64 bits
    if Length(aData) < 126 then            //7 bit, 128
      iByte := iByte + Length(aData)
    else if Length(aData) < 1 shl 16 then  //16 bit, 65536
      iByte := iByte + 126
    else
      iByte := iByte + 127;
    strmData.Write(iByte, SizeOf(iByte));

    //Extended payload length?
    if Length(aData) >= 126 then
    begin
      //If 126, the following 2 bytes interpreted as a 16-bit unsigned integer are the payload length
      if Length(aData) < 1 shl 16 then  //16 bit, 65536
      begin
        rLength.Lo := Length(aData);
        iByte := rLength.Bytes[1];
        strmData.Write(iByte, SizeOf(iByte));
        iByte := rLength.Bytes[0];
        strmData.Write(iByte, SizeOf(iByte));
      end
      else
      //If 127, the following 8 bytes interpreted as a 64-bit unsigned integer
      // the most significant bit MUST be 0
      // are the payload length
      begin
        rLength := Int64Rec(Int64(Length(aData)));
        for i := 7 downto 0 do
        begin
          iByte := rLength.Bytes[i];
          strmData.Write(iByte, SizeOf(iByte));
        end;
      end
    end;

    //Masking-key: 0 or 4 bytes; Note: Clients must apply a mask
    rMask.MaskAsInt := Random(MaxInt);
    strmData.Write(rMask.MaskAsBytes[0], SizeOf(Byte));
    strmData.Write(rMask.MaskAsBytes[1], SizeOf(Byte));
    strmData.Write(rMask.MaskAsBytes[2], SizeOf(Byte));
    strmData.Write(rMask.MaskAsBytes[3], SizeOf(Byte));

    //write header
    strmData.Position := 0;
    TIdStreamHelper.ReadBytes(strmData, bData);

    //Mask?
    iPos := 0;
    iDataLength := Length(aData);
    while iPos < iDataLength do
    begin
      iByte := aData[iPos] xor rMask.MaskAsBytes[iPos mod 4]; //apply mask
      aData[iPos] := iByte;
      inc(iPos);
    end;

    AppendBytes(bData, aData);
    ioffset := 0;
    repeat
      Result := inherited WriteDataToTarget(bdata, iOffset, (Length(bData) - ioffset));
      if Result < 0 then
        break;
      Inc(ioffset, Result);
    until ioffset >= Length(bData);
  finally
    strmData.Free;
  end;
end;

{ TIdHTTPWebsocketClient }

procedure THTTPWebsocketClient.AfterConstruction;
begin
  inherited;
  FHash := TIdHashSHA1.Create;

  IOHandler := TWebsocketIOHandler.Create(nil);
  IOHandler.UseNagle := False;
  ManagedIOHandler := True;

  FWriteTimeout  := 2000;
end;

function THTTPWebsocketClient.CheckConnection: Boolean;
begin
  Result := False;
  try
    if (IOHandler <> nil) and
       not IOHandler.ClosedGracefully and
      IOHandler.Connected then
    begin
      IOHandler.CheckForDisconnect(True, True);
      Result := True;
    end;
  except
    Disconnect(False);
    if Assigned(OnDisConnected) then
      OnDisconnected(Self);
  end;
end;

procedure THTTPWebsocketClient.Connect;
begin
  if Connected then
    UpgradeToWebsocket
  else
  begin
    if (IOHandler <> nil) then
      IOHandler.Clear;

    inherited Connect;
  end;
end;

destructor THTTPWebsocketClient.Destroy;
begin
  FreeAndNil(FHash);
  inherited;
end;

procedure THTTPWebsocketClient.Disconnect(ANotifyPeer: Boolean);
begin
  if IOHandler <> nil then
  begin
    IOHandler.IsWebsocket := False;
    inherited Disconnect(ANotifyPeer);

    IOHandler.Clear;
  end;
end;

procedure THTTPWebsocketClient.DoOnTextData(const Data: UTF8String);
begin
  if Assigned(FOnTextData) then
    FOnTextData(self, Data);
end;

function THTTPWebsocketClient.GetIOHandlerWS: TWebsocketIOHandler;
begin
  Result := inherited IOHandler as TWebsocketIOHandler
end;

function THTTPWebsocketClient.HttpUrl: string;
var
  Url : TIdURI;
begin
  // change ws:// to http://
  Url := TIdURI.Create(FUrl);
  try
    Url.Protocol := 'http';
    Result := Url.GetFullURI();
  finally
    FreeAndNil(Url);
  end;
end;

procedure THTTPWebsocketClient.UpgradeToWebsocket;
begin
  if IOHandler = nil then
    Connect
  else if not IOHandler.IsWebsocket then
    InternalUpgradeToWebsocket;
end;

procedure THTTPWebsocketClient.InternalUpgradeToWebsocket;
var
  strmResponse: TMemoryStream;
  sKey, sResponseKey: string;
begin
  Assert((IOHandler = nil) or not IOHandler.IsWebsocket);

  strmResponse := TMemoryStream.Create;
  try
    //reset pending data
    if IOHandler <> nil then
    begin
      if IOHandler.IsWebsocket then
        Exit;
      IOHandler.Clear;
    end;

    Request.Clear;
    Request.CustomHeaders.Clear;

    //Connection: Upgrade
    Request.Connection := 'Upgrade';
    //Upgrade: websocket
    Request.CustomHeaders.Add('Upgrade:websocket');

    //Sec-WebSocket-Key
    sKey := TIdEncoderMIME.EncodeString(RandomStr(16));
    Request.CustomHeaders.AddValue('Sec-WebSocket-Key', sKey);
    //Sec-WebSocket-Version: 13
    Request.CustomHeaders.AddValue('Sec-WebSocket-Version', '13');
    Request.CustomHeaders.AddValue('Sec-WebSocket-Extensions', '');

    Request.CacheControl := 'no-cache';
    Request.Pragma := 'no-cache';
    Request.Host := Format('Host:%s:%d',[Host,Port]);
    Request.CustomHeaders.AddValue('Origin', Format('http://%s:%d',[Host,Port]) );

    Get(HttpUrl, strmResponse, [101]);

    //'HTTP/1.1 101 Switching Protocols'
    if Response.ResponseCode <> 101 then
      raise EIdWebSocketHandleError.CreateFmt('Error while upgrading: "%d: %s"',[Response.ResponseCode, Response.ResponseText]);

    //connection: upgrade
    if not SameText(Response.Connection, 'upgrade') then
      raise EIdWebSocketHandleError.CreateFmt('Connection not upgraded: "%s"',[Response.Connection]);

    //upgrade: websocket
    if not SameText(Response.RawHeaders.Values['upgrade'], 'websocket') then
      raise EIdWebSocketHandleError.CreateFmt('Not upgraded to websocket: "%s"',[Response.RawHeaders.Values['upgrade']]);

    //handshake key
    sResponseKey := Trim(sKey) +                                         //... "minus any leading and trailing whitespace"
                    '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';              //special GUID
    sResponseKey := TIdEncoderMIME.EncodeBytes(                          //Base64
                         FHash.HashString(sResponseKey) );               //SHA1

    if not SameText(Response.RawHeaders.Values['sec-websocket-accept'], sResponseKey) then
      raise EIdWebSocketHandleError.Create('Invalid key handshake');

    //upgrade succesful
    IOHandler.IsWebsocket := True;

  finally
    Request.Clear;
    Request.CustomHeaders.Clear;
    strmResponse.Free;
  end;

  //default 2s write timeout
  //http://msdn.microsoft.com/en-us/library/windows/desktop/ms740532(v=vs.85).aspx
  if Connected then
    IOHandler.Binding.SetSockOpt(SOL_SOCKET, SO_SNDTIMEO, Self.WriteTimeout);
end;


function THTTPWebsocketClient.MakeImplicitClientHandler: TIdIOHandler;
begin
  Result := TWebsocketIOHandler.Create(nil);
end;

procedure THTTPWebsocketClient.Ping;
begin
  if IOHandler.IsWebsocket then
    IOHandler.WriteData(nil, wdcPing);
end;

procedure THTTPWebsocketClient.ReadAndProcessData;
var
  strmEvent: TMemoryStream;
  swstext: UTF8String;
  wscode: TWSDataCode;
begin
  strmEvent := nil;
  try
    //try to process all events
    while IOHandler.HasData or
          (IOHandler.Connected and
           IOHandler.Readable(0)) do     //has some data
    begin
      if strmEvent = nil then
        strmEvent := TMemoryStream.Create;
      strmEvent.Clear;

      //first is the data type TWSDataType(text or bin)
      wscode := TWSDataCode(IOHandler.ReadUInt32);
      if not (wscode in [wdcText, wdcBinary, wdcPing, wdcPong]) then
        Continue;

      // size + data = stream
      IOHandler.ReadStream(strmEvent);

      //ping/pong - игнорируем
      if wscode in [wdcPing, wdcPong] then
        Continue;

      strmEvent.Position := 0;
      if wscode = wdcBinary then
      begin
        //?? not implemented
      end
      else if wscode = wdcText then
      begin
        SetLength(swstext, strmEvent.Size);
        strmEvent.Read(swstext[1], strmEvent.Size);
        if Length(swstext)>0 then
          DoOnTextData(swstext);
      end;
    end;
  finally
    strmEvent.Free;
  end;
end;

procedure THTTPWebsocketClient.SetIOHandlerWS(const Value: TWebsocketIOHandler);
begin
  SetIOHandler(Value);
end;

procedure THTTPWebsocketClient.SetUrl(const Value: string);
var
  Url : TIdURI;
begin
  Url := TIdURI.Create(Value);
  try
    Host := Url.Host;
    Port := StrToInt(Url.Port);
    FUrl := Value;
  finally
    FreeAndNil(Url);
  end;
end;

procedure THTTPWebsocketClient.SetWriteTimeout(const Value: Integer);
begin
  FWriteTimeout := Value;
  if Connected then
    Self.IOHandler.Binding.SetSockOpt(SOL_SOCKET, SO_SNDTIMEO, Self.WriteTimeout);
end;

end.
