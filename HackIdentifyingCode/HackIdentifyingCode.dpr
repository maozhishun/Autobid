program HackIdentifyingCode;

uses
  Vcl.Forms,
  ufrmDemo in 'ufrmDemo.pas' {Form1},
  uIndentifyingCodeDefine in 'uIndentifyingCodeDefine.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
