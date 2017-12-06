unit FormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CommonConnection.Intf, Vcl.StdCtrls,
  Vcl.ExtCtrls, Spring.Container, Spring.Services, FrameConnectDB;

type
  TForm1 = class(TForm)
    pnDB: TPanel;
    Button1: TButton;
    rgConnectType: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure rgConnectTypeClick(Sender: TObject);
  private
    FDBConnectFrame: TDBConnectFrame;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReleaseGlobalDataProvider;
  cdpFDGlobal := ServiceLocator.GetService<IFDMemTableProvider>(CDP_IDENT_FD);
  cdpADOGlobal := ServiceLocator.GetService<IClientDataSetProvider>(CDP_IDENT_ADO);

  FDBConnectFrame := TDBConnectFrame.Create(Self);
  FDBConnectFrame.Parent := pnDB;
  pnDB.Align := alClient;

end;

procedure TForm1.rgConnectTypeClick(Sender: TObject);
begin
  if rgConnectType.ItemIndex = 0 then
    FDBConnectFrame.DBConnectType := dctFD
  else
    FDBConnectFrame.DBConnectType := dctADO;
end;

end.
