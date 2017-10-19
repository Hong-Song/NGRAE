program NGUtils;

uses
  Forms,
  NGUMain in 'NGUMain.pas' {FormMain},
  FormConnectDB in '..\common\DB\FormConnectDB.pas' {frmConnectDB};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
