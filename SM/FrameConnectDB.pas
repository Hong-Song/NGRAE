unit FrameConnectDB;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  CommonConnection.Intf;

type
  TDBConnectFrame = class(TFrame)
    lblHostName: TLabel;
    lblDBName: TLabel;
    lblConnectUsing: TLabel;
    lblLoginName: TLabel;
    lblPassword: TLabel;
    btnSQLLoadConnection: TSpeedButton;
    btnSQLSaveConnection: TSpeedButton;
    HostName: TComboBox;
    radWindowsAuthentication: TRadioButton;
    radSQLServerAuthentication: TRadioButton;
    AdminUserName: TEdit;
    AdminPassword: TEdit;
    DBName: TComboBox;
    btnSQLServerConnect: TBitBtn;
    procedure btnSQLServerConnectClick(Sender: TObject);
  private
    FDBConnectType: TDBConnectType;
    function GetDBConnectType: TDBConnectType;
    procedure SetDBConnectType(Value: TDBConnectType);

  public
    property DBConnectType: TDBConnectType read GetDBConnectType write SetDBConnectType;

    constructor Create(AOwner : TComponent); override;
  end;

implementation

{$R *.dfm}

{ TFrame1 }

procedure TDBConnectFrame.btnSQLServerConnectClick(Sender: TObject);
begin
  if FDBConnectType = dctFD then
  begin
    cdpFDGlobal.SetConnectionString(dtoSQLServer, HostName.Text, DBName.Text, '', AdminUserName.Text, AdminPassword.Text, radWindowsAuthentication.Checked);
    cdpFDGlobal.Connected := True;
  end
  else
  begin
    cdpADOGlobal.SetConnectionString(dtoSQLServer, HostName.Text, DBName.Text, '', AdminUserName.Text, AdminPassword.Text, radWindowsAuthentication.Checked);
    cdpADOGlobal.Connected := True;
  end;
end;


constructor TDBConnectFrame.Create(AOwner: TComponent);
begin
  inherited;
  //HostName.Text := 'hong-java\SQLEXPRESS,1433';
  FDBConnectType := dctFD;
end;

function TDBConnectFrame.GetDBConnectType: TDBConnectType;
begin
  Result := FDBConnectType;
end;

procedure TDBConnectFrame.SetDBConnectType(Value: TDBConnectType);
begin
  FDBConnectType := Value;
end;

end.
