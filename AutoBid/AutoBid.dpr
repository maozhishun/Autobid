program AutoBid;

uses
  Vcl.Forms,
  FCapInfo in 'FCapInfo.pas' {F_CapInfo},
  ufrmMain in 'ufrmMain.pas' {frmMain},
  ufrmOpDeclare in 'ufrmOpDeclare.pas' {frmOpDeclare},
  uHelp in 'uHelp.pas',
  uKeyHook in 'uKeyHook.pas',
  uPriceInfo in 'uPriceInfo.pas',
  USetting in 'USetting.pas',
  uStrategy in 'uStrategy.pas',
  uTestData in 'uTestData.pas',
  uCutScreen in 'uCutScreen.pas',
  GdiPlus2009 in 'GdiPlus2009.pas',
  GdiPlusHelpers in 'GdiPlusHelpers.pas',
  uColorUtil in '..\HackIdentifyingCode\uColorUtil.pas',
  uDownloadMgr in '..\HackIdentifyingCode\uDownloadMgr.pas',
  uIndentifyingCodeDefine in '..\HackIdentifyingCode\uIndentifyingCodeDefine.pas',
  uModeCompare in '..\HackIdentifyingCode\uModeCompare.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TF_CapInfo, F_CapInfo);
  Application.CreateForm(TfrmOpDeclare, frmOpDeclare);
  Application.Run;
end.
