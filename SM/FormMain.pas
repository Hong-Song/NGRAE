unit FormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CommonConnection.Intf, Vcl.StdCtrls,
  Vcl.ExtCtrls, Spring.Container, Spring.Services, FrameConnectDB, Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef, FireDAC.VCLUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, Data.Win.ADODB, Vcl.Grids,
  Vcl.DBGrids;

type
  TForm1 = class(TForm)
    pnLeft: TPanel;
    pnLeftTop: TPanel;
    btnFDOpen1: TButton;
    btnFDLoop: TButton;
    btnFDLoop2: TButton;
    DBGrid1: TDBGrid;
    pnRight: TPanel;
    pnRightTop: TPanel;
    btnADOOpen1: TButton;
    btnADOLoop: TButton;
    btnADOLoop2: TButton;
    DBGrid2: TDBGrid;
    ADOQuery1: TADOQuery;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    FDQuery1: TFDQuery;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnFDOpen1Click(Sender: TObject);
    procedure btnADOOpen1Click(Sender: TObject);
    procedure btnFDLoopClick(Sender: TObject);
    procedure btnADOLoopClick(Sender: TObject);
  private
    FFDDBConnectFrame, FADODBConnectFrame: TDBConnectFrame;
    FSQLString: string;
    FStartTime: DWORD;

    procedure ShowButtonCaption(Sender: TObject; iCount: Integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


//  if rgConnectType.ItemIndex = 0 then
//    FDBConnectFrame.DBConnectType := dctFD
//  else
//    FDBConnectFrame.DBConnectType := dctADO;

procedure TForm1.btnADOLoopClick(Sender: TObject);
begin
  ADOQuery1.First;
  FStartTime := GetTickCount;
  while not ADOQuery1.EOF do
    ADOQuery1.Next;
  ShowButtonCaption(Sender, ADOQuery1.RecordCount);
end;

procedure TForm1.btnADOOpen1Click(Sender: TObject);
begin
  ADOQuery1.Close;
  FStartTime := GetTickCount;
  ADOQuery1.Open();
  ShowButtonCaption(Sender, ADOQuery1.RecordCount);
end;

procedure TForm1.btnFDLoopClick(Sender: TObject);
begin
  FDQuery1.First;
  FStartTime := GetTickCount;
  while not FDQuery1.EOF do
    FDQuery1.Next;
  ShowButtonCaption(Sender, FDQuery1.RecordCount);
end;

procedure TForm1.btnFDOpen1Click(Sender: TObject);
begin
  FDQuery1.Close;
  FStartTime := GetTickCount;
  FDQuery1.Open();
  ShowButtonCaption(Sender, FDQuery1.RecordCount);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReleaseGlobalDataProvider;
  cdpFDGlobal := ServiceLocator.GetService<IFDMemTableProvider>(CDP_IDENT_FD);
  cdpADOGlobal := ServiceLocator.GetService<IClientDataSetProvider>(CDP_IDENT_ADO);

  FFDDBConnectFrame := TDBConnectFrame.Create(Self);
  FFDDBConnectFrame.DBConnectType := dctFD;
  FFDDBConnectFrame.Name := 'FD_DBConnectFrame';
  FFDDBConnectFrame.Parent := pnLeftTop;
  FFDDBConnectFrame.Align := alLeft;
  FFDDBConnectFrame.btnSQLServerConnectClick(nil);
  FDQuery1.Connection := TFDConnection(cdpFDGlobal.DBConnection.Connection);

  FADODBConnectFrame := TDBConnectFrame.Create(Self);
  FADODBConnectFrame.DBConnectType := dctADO;
  FADODBConnectFrame.Name := 'ADO_DBConnectFrame';
  FADODBConnectFrame.Parent := pnRightTop;
  FADODBConnectFrame.Align := alLeft;
  FADODBConnectFrame.btnSQLServerConnectClick(nil);
  ADOQuery1.Connection := TADOConnection(cdpADOGlobal.DBConnection.Connection);

  FSQLString := 'select  *  from TaskList'; //Row_Number() over(order by id) as RowID,
  FDQuery1.SQL.Text := FSQLString;
  ADOQuery1.SQL.Text := FSQLString;

  width := 1500;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  pnLeft.Width := width div 2;
end;

procedure TForm1.ShowButtonCaption(Sender: TObject; iCount: Integer);
begin
  TButton(Sender).Caption := Format('%s-%d ms-%d Records', [TButton(Sender).Name, (GetTickCount - FStartTime), iCount]);
end;

end.
