unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP, Vcl.StdCtrls, CallCenterNewClient,
  superobject, System.Actions, Vcl.ActnList;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    btnRun: TButton;
    cbUrl: TComboBox;
    btnAuthorize: TButton;
    btnStop: TButton;
    btnSend: TButton;
    ActionList1: TActionList;
    actRun: TAction;
    actStop: TAction;
    actSend: TAction;
    btnStatus: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnAuthorizeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actSendExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure btnStatusClick(Sender: TObject);
  private
    FWSClient : TCCNClient;
    procedure OnNotification(Sender:TObject; Obj:ISuperObject);
    procedure OnRequestTimeout(Sender:TObject; Obj:ISuperObject);
    procedure OnResponse(Sender:TObject; Request:ISuperObject; Response:ISuperObject);
    procedure OnConnect(Sender:TObject);
  public
    { Public declarations }

  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}
const
  JSON_Auth = '{"Type": "Authentication","Number": "75001", "Token": "03CF7743-7638-4892-AC6A-6D8ED72E2555"}';
//  JSON_Auth = '{"Type": "Authentication","Number": "75001", "Token": "BAD_TOKEN"}';
//  JSON_Auth = '{"Type": "Authentication","EmployeeOID_FullName": "Ћ€пин »ль€ Ёдуардович",'+
//              ' "RequestId": 1,"EmployeeOID_MiddleName": "Ћ€пин »ль€","EmployeeOID": 281646780744284,'+
//              '"Token": "BEED0769-6C53-4E85-A8E2-FE6B034167A1","Number": "75006"}';
  JSON_Status ='{ "Type": "Status", "Number": "1406"}';

procedure TForm2.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  actRun.Enabled := not FWSClient.IsRunning;
  actStop.Enabled := FWSClient.IsRunning;
  actSend.Enabled := FWSClient.IsRunning; //??
end;

procedure TForm2.actRunExecute(Sender: TObject);
begin
  if not FWSClient.IsRunning then
  begin
    FWSClient.Url :=  cbUrl.Text;
    FWSClient.Run;
  end;
end;

procedure TForm2.actSendExecute(Sender: TObject);
begin
  FWSClient.SendRequest(SO(Memo1.Text));
end;

procedure TForm2.actStopExecute(Sender: TObject);
begin
  if FWSClient.IsRunning then
    FWSClient.Stop;
end;

procedure TForm2.btnAuthorizeClick(Sender: TObject);
begin
  Memo1.Text := JSON_Auth;
end;

procedure TForm2.btnStatusClick(Sender: TObject);
begin
  Memo1.Text := JSON_Status;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := true;
  cbUrl.ItemIndex := 2;
  FWSClient := TCCNClient.Create;
  FWSClient.OnResponse := OnResponse;
  FWSClient.OnNotification := OnNotification;
  FWSClient.OnRequestTimedOut := OnRequestTimeout;
  FWSClient.OnConnect := OnConnect;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FWSClient.Free;
end;

procedure TForm2.FormResize(Sender: TObject);
begin
  Memo2.Height := ClientHeight - Memo2.Top - 5;
end;

procedure TForm2.OnConnect(Sender: TObject);
begin
 Memo1.Lines.Add('Connected to '+FWSClient.Url);
 FWSClient.SendRequest(SO(JSON_Auth));
end;

procedure TForm2.OnNotification(Sender: TObject; Obj: ISuperObject);
begin
  Memo2.Lines.Add('Notification'#13 + obj.AsJSon(true, false));
end;

procedure TForm2.OnRequestTimeout(Sender: TObject; Obj: ISuperObject);
begin
  Memo2.Lines.Add('Request timed out'#13 + obj.AsJSon(true, false));
end;

procedure TForm2.OnResponse(Sender: TObject; Request, Response: ISuperObject);
begin
  Memo2.Lines.Add('Request'#13 + Request.AsJSon(true, false));
  Memo2.Lines.Add('Response'#13 + Response.AsJSon(true, false));
end;

end.
