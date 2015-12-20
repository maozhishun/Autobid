library IndentifyingCodeKiller;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  System.SysUtils,
  System.Classes,
  uDownloadMgr in 'uDownloadMgr.pas',
  GdiPlus2009 in 'GdiPlus2009.pas',
  uColorUtil in 'uColorUtil.pas',
  uModeCompare in 'uModeCompare.pas',
  uIndentifyingCodeDefine in 'uIndentifyingCodeDefine.pas';

{$R *.res}

begin
end.
