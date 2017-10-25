unit FormConnectDB;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

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
    pnDBName: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

end.
