program NG;

uses
  Spring.Container,
  Spring.Services,
  Vcl.Forms,
  FormMain in 'FormMain.pas' {Form1},
  CommonConnection.Intf in '..\Common\CommonDBUtils\CommonConnection.Intf.pas',
  ConnectionProperties.Cls in '..\Common\CommonDBUtils\ConnectionProperties.Cls.pas',
  CommonDataProvider.FD in '..\Common\CommonDBUtils\CommonDataProvider.FD.pas',
  FDConnectionHelper in '..\Common\CommonDBUtils\FDConnectionHelper.pas',
  ParamsHelper in '..\Common\CommonDBUtils\ParamsHelper.pas',
  CommonDataProvider.ADO in '..\Common\CommonDBUtils\CommonDataProvider.ADO.pas',
  FrameConnectDB in 'FrameConnectDB.pas' {DBConnectFrame: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := (DebugHook<>0);

  Application.Initialize;

  GlobalContainer.Build;

  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
