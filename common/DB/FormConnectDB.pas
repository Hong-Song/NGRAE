unit FormConnectDB;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmConnectDB = class(TForm)
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
    btnConnectDB: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmConnectDB: TfrmConnectDB;

implementation

{$R *.dfm}

end.
