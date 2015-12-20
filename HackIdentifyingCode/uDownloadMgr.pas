unit uDownloadMgr;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, Winapi.Windows,
  Winapi.Messages, Winapi.UrlMon, Winapi.ActiveX, Vcl.ExtCtrls;

const
  WM_DownLoadCompleted = WM_USER + 1;

type
  TGetCodeByURLCallBack = procedure (const AURL: PAnsiChar; AURLLength: Integer;
    const ACode: PAnsiChar; ACodeLength: Integer; ASuccess: Boolean) of object;

  PDownloadItem = ^TDownloadItem;
  TDownloadItem = record
    URL: string;
    Result: string;
    RetryTimes: Word;
    TimeOut: Cardinal;
    CallBack: TGetCodeByURLCallBack;
  end;

  TDownLoadThread = class(TThread)
  private
    FDownloadItem: PDownloadItem;
    FNotifyHandle: THandle;
    procedure SetDownloadItem(const Value: PDownloadItem);
    procedure SetNotifyHandle(const Value: THandle);
  protected
    procedure Execute; override;
  public
    property DownloadItem: PDownloadItem read FDownloadItem write SetDownloadItem;
    property NotifyHandle: THandle read FNotifyHandle write SetNotifyHandle;
  end;

  TDownLoadMgr = class
  private
    FHandle: THandle;
    FDic: TDictionary<Cardinal, PDownloadItem>;//key: hThread
    procedure ClearDic;
    procedure WindowsProc(var Message: TMessage);
  public

    function DownLoad(const AURL: string; ACallBack: TGetCodeByURLCallBack;
      ARetryTimes: Word = 3; ATimeOut: Cardinal = 500): Boolean;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TBindStatusCallback = class(TInterfacedObject, IBindStatusCallback)
  private
    FRetrun: HRESULT;
    FTimeOut: TTimer;
    procedure DoOnTimeOut(Sender: TObject);

    function OnStartBinding(dwReserved: DWORD; pib: IBinding): HResult; stdcall;
    function GetPriority(out nPriority): HResult; stdcall;
    function OnLowResource(reserved: DWORD): HResult; stdcall;
    function OnProgress(ulProgress, ulProgressMax, ulStatusCode: ULONG;
      szStatusText: LPCWSTR): HResult; stdcall;
    function OnStopBinding(hresult: HResult; szError: LPCWSTR): HResult; stdcall;
    function GetBindInfo(out grfBINDF: DWORD; var bindinfo: TBindInfo): HResult; stdcall;
    function OnDataAvailable(grfBSCF: DWORD; dwSize: DWORD; formatetc: PFormatEtc;
      stgmed: PStgMedium): HResult; stdcall;
    function OnObjectAvailable(const iid: TGUID; punk: IUnknown): HResult; stdcall;

  public
    constructor Create(ATimeOut: Integer); reintroduce;
    destructor Destroy; override;
  end;


  function GetCodeByURLA(const AURL: PAnsiChar; AURLLength: Integer;
    ACallBack: TGetCodeByURLCallBack;
    ARetryTimes: Word = 3; ATimeOut: Cardinal = 500): Boolean; stdcall;

  function GetCodeByURL(const AURL: string; ACallBack: TGetCodeByURLCallBack;
    ARetryTimes: Word = 3; ATimeOut: Cardinal = 500): Boolean;

exports
  GetCodeByURL;

implementation

uses
  uColorUtil, IdHTTP;

var
  g_DownLoadMgr: TDownLoadMgr;


function GetCodeByURLA(const AURL: PAnsiChar; AURLLength: Integer;
    ACallBack: TGetCodeByURLCallBack;
    ARetryTimes: Word = 3; ATimeOut: Cardinal = 500): Boolean;
var
  LAnsiStringURL: AnsiString;
begin
  Result := False;
  SetLength(LAnsiStringURL, AURLLength);
  LAnsiStringURL := AURL;
  if Assigned(g_DownLoadMgr) then
    Result := g_DownLoadMgr.DownLoad(string(LAnsiStringURL), ACallBack, ARetryTimes, ATimeOut);
end;

function GetCodeByURL(const AURL: string; ACallBack: TGetCodeByURLCallBack;
    ARetryTimes: Word = 3; ATimeOut: Cardinal = 500): Boolean;
begin
  Result := False;
  if Assigned(g_DownLoadMgr) then
    Result := g_DownLoadMgr.DownLoad(AURL, ACallBack, ARetryTimes, ATimeOut);
end;

{ TDownLoadMgr }

procedure TDownLoadMgr.AfterConstruction;
begin
  inherited;
  FDic := TDictionary<Cardinal, PDownloadItem>.Create;
  FHandle := AllocateHWnd(WindowsProc);
end;

procedure TDownLoadMgr.BeforeDestruction;
begin
  DeallocateHWnd(FHandle);
  ClearDic;
  FDic.Free;
  inherited;
end;

procedure TDownLoadMgr.ClearDic;
var
  LThreadHandle: Cardinal;
  LItem: PDownloadItem;
begin
  for LThreadHandle in FDic.Keys do
  begin
    TerminateThread(LThreadHandle, 0);
    LItem := FDic.Items[LThreadHandle];
    Dispose(LItem);
  end;
  FDic.Clear;
end;

function TDownLoadMgr.DownLoad(const AURL: string; ACallBack: TGetCodeByURLCallBack;
  ARetryTimes: Word = 3; ATimeOut: Cardinal = 500): Boolean;
var
  LItem: PDownloadItem;
  LThread: TDownLoadThread;
begin
  Result := False;
  New(LItem);
  LItem.URL := AURL;
  LItem.Result := '';
  LItem.RetryTimes := ARetryTimes;
  LItem.TimeOut := ATimeOut;
  LItem.CallBack := ACallBack;

  LThread := TDownLoadThread.Create;
  LThread.DownloadItem := LItem;
  LThread.NotifyHandle := FHandle;
  FDic.Add(LThread.Handle, LItem);

  LThread.Resume;
end;

procedure TDownLoadMgr.WindowsProc(var Message: TMessage);
var
  LHandle: Cardinal;
  LDownloadResult: Boolean;
  LItem: PDownloadItem;
  LAnsiURL, LAnsiCode: AnsiString;
begin
  if Message.Msg = WM_DownLoadCompleted then
  begin
    LHandle := Message.WParam;
    LDownloadResult := Boolean(Message.LParam);


    LItem := FDic.Items[LHandle];
    //Call CallBack
    if Assigned(LItem.CallBack) then
    begin
      LAnsiURL := AnsiString(LItem.URL);
      LAnsiCode := AnsiString(LItem.Result);
      LItem.CallBack(PAnsiChar(LAnsiURL), Length(LAnsiURL),
        PAnsiChar(LAnsiCode), Length(LAnsiCode), LDownloadResult);
    end;

    Dispose(LItem);
    FDic.Remove(LHandle);
  end;
end;

{ TDownLoadThread }

procedure TDownLoadThread.Execute;
var
  LURL: string;
  LPath: string;
  LFileName: string;
  LDownloadResult: Boolean;
  LTryCount: Integer;
  LSplitStrings: TStringList;
  LIDHttp: TIdHTTP;
  LStream: TMemoryStream;
begin
  inherited;
  LURL := FDownLoadItem.URL;

  LSplitStrings := TStringList.Create;
  LSplitStrings.Delimiter := '/';
  LSplitStrings.DelimitedText := LURL;
  LFileName := LSplitStrings[LSplitStrings.Count - 1];
  LSplitStrings.Free;


  LPath := './code/download';
  LFileName := LPath + '/'  + LFileName;
  if not DirectoryExists(LPath) then
    ForceDirectories(LPath);
  if FileExists(LFileName) then
    DeleteFile(PChar(LFileName));



  LTryCount := 0;
  LDownloadResult := False;
  while not LDownloadResult and (LTryCount <= FDownloadItem.RetryTimes) do
  begin
    {
    LCallBack := TBindStatusCallback.Create(FDownloadItem.TimeOut);
    URLDownloadToFile(nil, PChar(LURL), PChar(LFileName), 0, LCallBack);
    }


    LIDHttp := TIdHTTP.Create(nil);
    LIDHttp.ReadTimeout :=  FDownloadItem.TimeOut;
    LIDHttp.ConnectTimeout := FDownloadItem.TimeOut;
    LStream := TMemoryStream.Create;
    try
      LIDHttp.Get(LURL, LStream);
      if (LIDHttp.ResponseCode < 300) and (LIDHttp.ResponseCode >= 200) then
        LStream.SaveToFile(LFileName);
    except
    end;
    LStream.Free;
    LIDHttp.Free;


    LDownloadResult := FileExists(LFileName);
    Inc(LTryCount);
  end;

  if LDownloadResult and FileExists(LFileName) then
  begin
    FDownloadItem.Result := GetCode(LFileName);
  end;

  PostMessage(NotifyHandle, WM_DownLoadCompleted, Self.Handle, Integer(LDownloadResult));
end;


procedure TDownLoadThread.SetDownloadItem(const Value: PDownloadItem);
begin
  FDownloadItem := Value;
end;

procedure TDownLoadThread.SetNotifyHandle(const Value: THandle);
begin
  FNotifyHandle := Value;
end;

{ TBindStatusCallback }

constructor TBindStatusCallback.Create(ATimeOut: Integer);
begin
  inherited Create;
  FRetrun := S_OK;
  FTimeOut := TTimer.Create(nil);
  FTimeOut.OnTimer := DoOnTimeOut;
  FTimeOut.Interval := ATimeOut;
  FTimeOut.Enabled := True;
end;

destructor TBindStatusCallback.Destroy;
begin
  FTimeOut.Free;
  inherited;
end;

procedure TBindStatusCallback.DoOnTimeOut(Sender: TObject);
begin
  FRetrun := E_ABORT;
end;

function TBindStatusCallback.GetBindInfo(out grfBINDF: DWORD;
  var bindinfo: TBindInfo): HResult;
begin
  Result := FRetrun;
end;

function TBindStatusCallback.GetPriority(out nPriority): HResult;
begin
  Result := FRetrun;
end;

function TBindStatusCallback.OnDataAvailable(grfBSCF, dwSize: DWORD;
  formatetc: PFormatEtc; stgmed: PStgMedium): HResult;
begin
  Result := FRetrun;
end;

function TBindStatusCallback.OnLowResource(reserved: DWORD): HResult;
begin
  Result := FRetrun;
end;

function TBindStatusCallback.OnObjectAvailable(const iid: TGUID;
  punk: IInterface): HResult;
begin
  Result := FRetrun;
end;

function TBindStatusCallback.OnProgress(ulProgress, ulProgressMax,
  ulStatusCode: ULONG; szStatusText: LPCWSTR): HResult;
begin
  Result := FRetrun;
end;

function TBindStatusCallback.OnStartBinding(dwReserved: DWORD;
  pib: IBinding): HResult;
begin
  Result := FRetrun;
end;

function TBindStatusCallback.OnStopBinding(hresult: HResult;
  szError: LPCWSTR): HResult;
begin
  Result := FRetrun;
end;

initialization
  g_DownLoadMgr := TDownLoadMgr.Create;

finalization
  g_DownLoadMgr.Free;

end.
