unit NGUMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, ComCtrls, ActnList, ImgList;

type
  TFormMain = class(TForm)
    pnLeft: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    ImageListMenu: TImageList;
    ActionList1: TActionList;
    aCreateDropDB: TAction;
    aUpgradeDB: TAction;
    aSecureDB: TAction;
    aBackupRestoreDB: TAction;
    imgMain: TImage;
    procedure aCreateDropDBUpdate(Sender: TObject);
    procedure aCreateDropDBExecute(Sender: TObject);
    procedure aUpgradeDBExecute(Sender: TObject);
    procedure aSecureDBExecute(Sender: TObject);
    procedure aBackupRestoreDBExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.aCreateDropDBUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TFormMain.aBackupRestoreDBExecute(Sender: TObject);
begin
//
end;

procedure TFormMain.aCreateDropDBExecute(Sender: TObject);
begin
//
end;

procedure TFormMain.aSecureDBExecute(Sender: TObject);
begin
//
end;

procedure TFormMain.aUpgradeDBExecute(Sender: TObject);
begin
//
end;

end.
