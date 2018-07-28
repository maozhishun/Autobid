unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, ComCtrls, OleCtrls, SHDocVw,
  Generics.Collections, uPriceInfo, Vcl.Samples.Spin;


const
  USER_RECEIVECAPINFO = WM_USER + 1;
  USER_RECEIVEIMAGEURL = WM_USER + 2;
type
  TGetCodeByURLCallBack = procedure (const AURL: PAnsiChar; AURLLength: Integer;
    const ACode: PAnsiChar; ACodeLength: Integer; ASuccess: Boolean) of object;

  TCatchPriceMode = (cpmCutScreen, cpmCapPacket, cpmUnknown);
  TfrmMain = class(TForm)
    Timer1: TTimer;
    Label3: TLabel;
    etFilter: TEdit;
    Label5: TLabel;
    etCurPrice: TEdit;
    BitBtn2: TBitBtn;
    ckTop: TCheckBox;
    ckP: TCheckBox;
    ckCapAll: TCheckBox;
    btnOpDeclare: TBitBtn;
    grpManualStrategy: TGroupBox;
    Timer2: TTimer;
    Button1: TButton;
    Button2: TButton;
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    TabSheet1: TTabSheet;
    grpAutoStrategy: TGroupBox;
    btnPrintAutoStrategy: TButton;
    pnlToolBar: TPanel;
    edtKeyInputInterval: TEdit;
    Label6: TLabel;
    chkIsCopyMode: TCheckBox;
    edtFilterURL: TEdit;
    lbSecondBidCount: TLabel;
    stat1: TStatusBar;
    tsHackIndex: TTabSheet;
    btnPriceChange: TButton;
    mmoHackIndex: TMemo;
    lblPriceChange: TLabel;
    edtThousands: TSpinEdit;
    btnSetThousand: TButton;
    btnCutScreen: TButton;
    tmrCutScreen: TTimer;
    btnFontSettings: TButton;
    FontDialog1: TFontDialog;
    seTimerTestInterval: TSpinEdit;
    Label1: TLabel;
    rgCommitSettings: TRadioGroup;
    procedure Timer1Timer(Sender: TObject);
    procedure btCapPacketClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure ckTopClick(Sender: TObject);
    procedure btnOpDeclareClick(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnPrintAutoStrategyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkIsCopyModeClick(Sender: TObject);
    procedure edtKeyInputIntervalExit(Sender: TObject);
    procedure btnPriceChangeClick(Sender: TObject);
    procedure btnSetThousandClick(Sender: TObject);
    procedure btnCutScreenClick(Sender: TObject);
    procedure tmrCutScreenTimer(Sender: TObject);
    procedure btnFontSettingsClick(Sender: TObject);
    procedure seTimerTestIntervalChange(Sender: TObject);
    procedure rgCommitSettingsClick(Sender: TObject);
    procedure stat1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
  private
    FHotKeyDict: TDictionary<Integer, TButton>;
    FThreadHandles: TList<THandle>;
    FURLPool: TStringList;
    FCatchPriceMode: TCatchPriceMode;
//    procedure StartCap();
    procedure InitStrategy;
    procedure UpdatePriceInfo(APrice: TPriceInfo);
    procedure UpdateCommitStatus;
    procedure UpdateServerTimeDelta;
    procedure UpdatePriceChange;
    procedure DoOnStrategyClick(Sender: TObject);
    procedure DoOnHotKeyEvent(AKey: Cardinal; var AHandle: Boolean);
//    procedure GetCodeByURLCallBack(const AURL: PAnsiChar; AURLLength: Integer;
//    const ACode: PAnsiChar; ACodeLength: Integer; ASuccess: Boolean);
  protected
    procedure CreateHandle; override;
    procedure WM_USERRECEIVECAPINFO(var Message: TMessage); message USER_RECEIVECAPINFO;
  public
    procedure AnalyzePacket(strData:string);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

var
  frmMain: TfrmMain;

implementation
uses
  FCapInfo, ufrmOpDeclare, uStrategy, uKeyHook, uHelp, USetting, DateUtils,
  uTestData, SyncObjs, Winapi.UrlMon, uCutScreen, uDownloadMgr, uModeCompare;

{$R *.dfm}

var
  CriticalSection: TCriticalSection;
  G_CapInfo: string;
  G_CapInfoURL: string;
  G_Handle: THandle = 0;

//type
//  function startNicCap(filter:PAnsiChar;isSelfOnly:boolean;interval:integer;capAll:integer;
//    capCallback:pointer;errCallback:pointer):integer;stdcall;external 'PPPriceDetect.dll';

//  procedure initWinSocket();stdcall;external 'PPPriceDetect.dll';

//  function GetCodeByFile(const AFileName: PAnsiChar; ALength: Integer;
//    var AOutLength: Integer): PAnsiChar; stdcall; external 'IndentifyingCodeKiller.dll';
//  function GetCodeByURL(const AURL: PAnsiChar; AURLLength: Integer;
//    ACallBack: TGetCodeByURLCallBack;
//    ARetryTimes: Word = 3; ATimeOut: Cardinal = 500): Boolean; stdcall; external 'IndentifyingCodeKiller.dll';



function UTF8ToWide(const US: UTF8String): WideString;
var
  len: integer;
  ws: WideString;
begin
  Result:='';
  if (Length(US) = 0) then  exit;
  len:=MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(US), -1, nil, 0);
  SetLength(ws, len);
  MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(US), -1, PWideChar(ws), len);
  Result:=ws;
end;

function WideToAnsi(const WS: WideString): AnsiString;
var
  len: integer;
  s: AnsiString;
begin
  Result:='';
  if (Length(WS) = 0) then exit;
  len:=WideCharToMultiByte(CP_ACP, 0, PWideChar(WS), -1, nil, 0, nil, nil);
  SetLength(s, len);
  WideCharToMultiByte(CP_ACP, 0, PWideChar(WS), -1, PAnsiChar(s), len, nil, nil);
  Result:=s;
end;

function UTF8ToAnsi(const S: UTF8String): AnsiString;
begin
  Result:=WideToAnsi(UTF8ToWide(S));
end;

procedure capCallback(priceStr:PAnsiChar; strLen:integer);stdcall;
var
  pp : AnsiString;
  tempStr : UTF8String;
begin
  SetLength(tempStr,strLen+1);
  strlcopy(PAnsiChar(tempStr),PAnsiChar(priceStr),strlen);
//  tempStr := priceStr;
  pp := UTF8ToAnsi(tempStr);

  CriticalSection.Enter;
  G_CapInfo := string(pp);
  CriticalSection.Leave;
  PostMessage(G_Handle, USER_RECEIVECAPINFO, 0, 0);
end;

procedure capCallbackURL(priceStr:PAnsiChar; strLen:integer);stdcall;
var
  pp : AnsiString;
  tempStr : UTF8String;
  LURL: string;
begin
  SetLength(tempStr,strLen+1);
  strlcopy(PAnsiChar(tempStr),PAnsiChar(priceStr),strlen);
//  tempStr := priceStr;
  pp := UTF8ToAnsi(tempStr);

  LURL := Trim(string(pp));
  if Pos('imgs/b/', LURL) > 0 then
  begin
    CriticalSection.Enter;
    G_CapInfoURL := LURL;
    CriticalSection.Leave;
    PostMessage(G_Handle, USER_RECEIVEIMAGEURL, 0, 0);
  end;
end;

procedure errorCallback(errInfo:PansiChar);stdcall;
begin
//  F_Main.mmLog.Lines.Add(errInfo);
end;

{
procedure TfrmMain.StartCap;
var
  liCapAll : integer;
begin
  if  self.ckCapAll.checked then begin
    liCapAll :=1;
  end else begin
    liCapAll :=0;
  end;
  startNicCap(PAnsiChar(AnsiString(Trim(etFilter.Text))),ckP.Checked,1,liCapAll,@capCallback,@capCallback);

  startNicCap(PAnsiChar(AnsiString(Trim(edtFilterURL.Text))),ckP.Checked,1,liCapAll,@capCallbackURL,@capCallbackURL);

  btCapPacket.Enabled := false;

  FCatchPriceMode := cpmCapPacket;
end;
}

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  stat1.Panels.Items[0].Text := '����ʱ��: ' + TimeToStr(time);
end;


procedure TfrmMain.Timer2Timer(Sender: TObject);
var
  LString: string;
begin
  LString := GetTestData;
  if Length(LString) <> 0 then
  begin
    CriticalSection.Enter;
    G_CapInfo := LString;
    CriticalSection.Leave;
    SendMessage(G_Handle, USER_RECEIVECAPINFO, 0, 0);
  end;
end;

procedure TfrmMain.tmrCutScreenTimer(Sender: TObject);
var
  LString: string;
  LPriceInfo: TPriceInfo;
begin
  if FCatchPriceMode = cpmCutScreen then
  begin
    LString := g_CutScreenMgr.GetCutScreenPrice;
    if Length(LString) > 0 then
    begin
      LPriceInfo := TPriceInfo.Create('');
      LPriceInfo.DisplayPrice := StrToInt(LString);
      LPriceInfo.DisplayTime := Now;
      g_PriceInfoManager.Add(LPriceInfo);
      UpdatePriceInfo(LPriceInfo);
      F_CapInfo.AppendExtInfo('�����۸�:'+ LString);
    end;
  end;
end;


procedure TfrmMain.UpdateCommitStatus;
begin
  if g_StrategyManager.IsReadyCommit then
  begin
    stat1.Panels.Items[1].Text := '����״̬���������';
  end
  else
  begin
    stat1.Panels.Items[1].Text := '����״̬��δ����';
  end;
end;

procedure TfrmMain.UpdatePriceChange;
begin
  if g_PriceInfoManager.HackIndex.PriceChanged then
    lblPriceChange.Caption := '��'
  else
    lblPriceChange.Caption := '����'
end;

procedure TfrmMain.UpdatePriceInfo(APrice: TPriceInfo);
//var
//  LCalcWaitTime: Double;
//  LTime: TDateTime;
begin
  try
//    lbServerTime.Caption := FormatDateTime('hh:mm:ss', APrice.ServerTime);

    if APrice.BidStage in [bsFirst, bsSecond] then
    begin
//      lbCarCount.Caption := IntToStr(APrice.LicenseCount);
//      lbCustomer.Caption := 'Ͷ��������'+inttostr(APrice.TotalBidderCount);
      etCurPrice.Text := IntToStr(APrice.DisplayPrice);
//      etCurQLen.Text := IntToStr(APrice.UnprocessCount);
//      lbFirstBidCount.Caption := inttostr(g_PriceInfoManager.FirstBidCount);
//      etNewBidCount.Text := IntToStr(g_PriceInfoManager.NewBidCount);
//      lbCalcTime.Caption := TimeToStr(APrice.DisplayTime);
//      LCalcWaitTime := (g_PriceInfoManager.CalcWaitTime);
//      if LCalcWaitTime < 0 then
//        lblWaitTime.Caption := 'N/A'
//      else
//        lblWaitTime.Caption := FormatFloat('0.00', LCalcWaitTime);
//      lblBlock.Visible := g_PriceInfoManager.IsBlock;
//      lblAcceptPrice.Visible := g_PriceInfoManager.IsAcceptPrice;


//      LTime := IncSecond(APrice.ServerTime, Round(LCalcWaitTime));
//      lblBidTime.Caption := FormatDateTime('hh:mm:ss', LTime);


//      if LTime >= g_StrategyManager.LastBidTime then
//      begin
//        lblBidTime.Font.Color := clRed;
//        lblBidTime.Font.Style := lblBidTime.Font.Style + [fsBold];
//      end;
//      F_CapInfo.AppendExtInfo(lbServerTime.Caption + '------' + lblBidTime.Caption);
    end;
  Except
  end;
end;

procedure TfrmMain.UpdateServerTimeDelta;
begin
//  lblSyncTime.Caption :=
//    FormatFloat('####.##', g_PriceInfoManager.ServerTimeDelta * 24 * 3600) + '��';
end;

procedure TfrmMain.WM_USERRECEIVECAPINFO(var Message: TMessage);
var
  LStr: string;
begin
  CriticalSection.Enter;
  LStr := G_CapInfo;
  G_CapInfo := '';
  CriticalSection.Leave;
  if Length(LStr) > 0 then
    AnalyzePacket(LStr);
end;

procedure TfrmMain.btCapPacketClick(Sender: TObject);
begin
//  StartCap();
end;

procedure TfrmMain.btnOpDeclareClick(Sender: TObject);
var
  Lfrm: TfrmOpDeclare;
begin
  Lfrm := TfrmOpDeclare.Create(nil);
  Lfrm.ShowModal;
  Lfrm.Free;
end;

procedure TfrmMain.btnPriceChangeClick(Sender: TObject);
begin
  g_PriceInfoManager.HackIndex.PriceChanged :=
    not g_PriceInfoManager.HackIndex.PriceChanged;
  UpdatePriceChange;
end;

procedure TfrmMain.btnPrintAutoStrategyClick(Sender: TObject);
var
  I: Integer;
  LStrategy: TAutomaticStrategy;
begin
  for I := 0 to g_StrategyManager.Count - 1 do
  begin
    case g_StrategyManager[I].Type_ of
      stAutomatic:
        begin
          LStrategy := TAutomaticStrategy(g_StrategyManager[I]);
          F_CapInfo.AppendExtInfo('');
          F_CapInfo.AppendExtInfo('���Զ����ԡ�');
          F_CapInfo.AppendExtInfo('��ʼʱ��:' + FormatDateTime('YYYYMMDD hhmmss zzz', LStrategy.StartTime));
          F_CapInfo.AppendExtInfo('�Ӽ�:' + IntToStr(LStrategy.AddPrice));
          F_CapInfo.AppendExtInfo('�ύʱ��:' + FormatDateTime('YYYYMMDD hhmmss zzz', LStrategy.CommitTime));
          F_CapInfo.AppendExtInfo('�ύ�۸�:' + IntToStr(LStrategy.CommitAddPrice));
        end;
    end;
  end;
  F_CapInfo.Show();
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  FCatchPriceMode := cpmCapPacket;
  Timer2.Enabled := True;
end;

procedure SendTestData;
var
  LString: string;
begin
  while True do
  begin
    LString := GetTestData;
    if Length(LString) <> 0 then
    begin
      CriticalSection.Enter;
      G_CapInfo := LString;
      CriticalSection.Leave;
      SendMessage(G_Handle, USER_RECEIVECAPINFO, 0, 0);
    end;
    Sleep(1000);
  end;
end;


procedure TfrmMain.Button2Click(Sender: TObject);
var
//  LID: THandle;
  LID2: Cardinal;
begin
  //Delphi 2010
  //CreateThread(nil, 0, @SendTestData, nil, 0, LID);

  //Delphi XE2
  CreateThread(nil, 0, @SendTestData, nil, 0, LID2);
end;

procedure TfrmMain.btnSetThousandClick(Sender: TObject);
begin
  g_PriceInfoManager.ManualThousands := edtThousands.Value;
end;

procedure TfrmMain.btnCutScreenClick(Sender: TObject);
begin
//  ShowMessage(g_CutScreenMgr.GetCutScreenPrice);

  FCatchPriceMode := cpmCutScreen;
  tmrCutScreen.Enabled := True;
//  btnCutScreen.Enabled := False;
  //ShowMessage(g_CutScreenMgr.GetCutScreenPrice);
end;

procedure TfrmMain.btnFontSettingsClick(Sender: TObject);
begin
  FontDialog1.Font.Name := g_FontModeCompareMgr.FontName;
  FontDialog1.Font.Height := g_FontModeCompareMgr.FontHeight;
  FontDialog1.Font.Style := g_FontModeCompareMgr.FontStyles;
  if FontDialog1.Execute(Self.Handle) then
  begin
    g_FontModeCompareMgr.SetFontMode(
      FontDialog1.Font.Name,
      FontDialog1.Font.Height,
      FontDialog1.Font.Style);
  end;
end;

procedure TfrmMain.BeforeDestruction;
var
  I: Integer;
begin
  inherited;

  for I := 0 to FThreadHandles.Count - 1 do
    TerminateThread(FThreadHandles[I], 0);
  FThreadHandles.Free;

  DetachEvent;
  FURLPool.Free;
  FHotKeyDict.Free;
end;

procedure TfrmMain.BitBtn2Click(Sender: TObject);
begin
  F_CapInfo.Show();
end;

procedure TfrmMain.AfterConstruction;
begin
  inherited;
  FCatchPriceMode := cpmUnknown;
  FHotKeyDict := TDictionary<Integer, TButton>.Create;
  FURLPool := TStringList.Create;
  FThreadHandles := TList<THandle>.Create;

  Timer1.Enabled := True;
//  initWinSocket();
  self.Top := 0;
  self.Left := 0;
  InitStrategy;

  AttachEvent(DoOnHotKeyEvent);

  PageControl1.ActivePageIndex := 0;


  seTimerTestInterval.Value := g_AppSettings.TestTimeInterval;
  Timer2.Interval := g_AppSettings.TestTimeInterval;

  rgCommitSettings.ItemIndex := Ord(g_AppSettings.CommitKeyInput);
end;

procedure TfrmMain.AnalyzePacket(strData: string);
var
  strList : TStrings;
  LPriceInfo: TPriceInfo;
begin
  if FCatchPriceMode = cpmCapPacket then
  begin
    Application.ProcessMessages();
    strList := TStringList.Create();
    try
      g_priceInfomanager.HackIndex.AddInfo(strData);
      if PageControl1.ActivePageIndex = 1 then
        g_priceInfomanager.HackIndex.RefreshInfo(mmoHackIndex.Lines);
      LPriceInfo := g_priceInfomanager.BuildPriceInfo(strData);
      if Assigned(LPriceInfo) then
      begin
        g_PriceInfoManager.Add(LPriceInfo);
        UpdatePriceInfo(LPriceInfo);
        F_CapInfo.AppendExtInfo(strData);
      end;
    finally
      strList.Free;
    end;
  end;
end;


procedure TfrmMain.chkIsCopyModeClick(Sender: TObject);
begin
  g_StrategyManager.IsCopyMode := chkIsCopyMode.Checked;
end;

procedure TfrmMain.ckTopClick(Sender: TObject);
begin
  if ckTop.Checked then begin
     self.FormStyle := fsStayOnTop;
  end else begin
     self.FormStyle := fsNormal;
  end;
end;

procedure TfrmMain.CreateHandle;
begin
  inherited;
  G_Handle := Self.Handle;
end;

procedure TfrmMain.DoOnHotKeyEvent(AKey: Cardinal; var AHandle: Boolean);
var
  LButton: TButton;
begin
  case g_AppSettings.CommitKeyInput of
    ckitCtrlEnter:
      if GetKeyState(VK_CONTROL) < 0 then
      begin
        case AKey of
          VK_RETURN: //Ctrl+Enter�ύ��֤��
            begin
              g_StrategyManager.Commit;
              AHandle := True;
            end;
        end;
      end;
    ckitEnter:
      case AKey of
        VK_RETURN: //Ctrl+Enter�ύ��֤��
        begin
          g_StrategyManager.Commit;
          AHandle := True;
        end;
      end;
  end;


  if GetKeyState(VK_CONTROL) < 0 then
  begin
    case AKey of
      VK_PRIOR:
        begin
          g_StrategyManager.IsReadyCommit := True;
          UpdateCommitStatus;
        end;
      VK_NEXT:
        begin
          g_StrategyManager.IsReadyCommit := False;
          UpdateCommitStatus;
        end;
      96..105:  //Ctrl+С����0-9
        begin
          if FHotKeyDict.ContainsKey(AKey - 96) then
          begin
            LButton := FHotKeyDict.Items[AKey - 96];
            if Assigned(LButton) then
            begin
              g_StrategyManager.InvokeStrategy(g_StrategyManager[LButton.Tag]);
              AHandle := True;
            end;
          end;
        end;
    end;
  end;
end;

procedure TfrmMain.DoOnStrategyClick(Sender: TObject);
var
  LButton: TButton;
begin
  LButton := TButton(Sender);
  g_StrategyManager.InvokeStrategy(g_StrategyManager[LButton.Tag]);
end;

procedure TfrmMain.edtKeyInputIntervalExit(Sender: TObject);
var
  LInterval: Integer;
begin
  LInterval := StrToIntDef(edtKeyInputInterval.Text, 0);
  if (LInterval <= 0) or (LInterval > 1000) then
  begin
    ShowMessage('������0��1000֮�������');
  end
  else
    g_StrategyManager.KeyInputInterval := LInterval;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  UpdateServerTimeDelta;
  UpdatePriceChange;
  edtKeyInputInterval.Text := IntToStr(g_StrategyManager.KeyInputInterval);
  chkIsCopyMode.Checked := g_StrategyManager.IsCopyMode;
end;

{
procedure TfrmMain.GetCodeByURLCallBack(const AURL: PAnsiChar; AURLLength: Integer;
    const ACode: PAnsiChar; ACodeLength: Integer; ASuccess: Boolean);
var
  LAnsiURL, LAnsiCode: AnsiString;
  LCode: string;
begin
  SetLength(LAnsiURL, AURLLength);
  SetLength(LAnsiCode, ACodeLength);
  LAnsiURL := AURL;
  LAnsiCode := ACode;

  //ShowMessage('URL:'+ string(LAnsiURL) + #13#10 + 'Code:' + string(LAnsiCode) + #13#10 + 'Success:' +
  //  BoolToStr(ASuccess));

  LCode := string(LAnsiCode);
  if Length(LCode) > 0 then
  begin
    g_StrategyManager.InputIndentifyCode(LCode);

    UpdateCommitStatus;
  end;
end;
}

procedure TfrmMain.InitStrategy;
const
  Button_Indent = 15;
var
  I: Integer;
  LButton: TButton;
  LManual: TManualStrategy;
  LLeft: Integer;
  LTop: Integer;
  LAutoInvokeThread: TAutoInvokeThread;
begin
  LLeft := Button_Indent;
  LTop := (grpManualStrategy.Height - 25) div 2;
  //�ֶ����ԼӰ�ť
  for I := 0 to g_StrategyManager.Count - 1 do
  begin
    case g_StrategyManager[I].Type_ of
      stAutomatic:
        begin
          LAutoInvokeThread := TAutoInvokeThread.Create(False);
          LAutoInvokeThread.Strategy := TAutomaticStrategy(g_StrategyManager[I]);
          LAutoInvokeThread.Resume;

          FThreadHandles.Add(LAutoInvokeThread.Handle);
        end;
      stManual:
        begin
          LManual := TManualStrategy(g_StrategyManager[I]);
          LButton := TButton.Create(Self);
          LButton.Parent := grpManualStrategy;
          LButton.Caption := LManual.Caption;
          LButton.OnClick := DoOnStrategyClick;
          LButton.Tag := I;
          LButton.Left := LLeft;
          LButton.Top := LTop;
          LLeft := LLeft + LButton.Width + Button_Indent;
          FHotKeyDict.Add(LManual.HotKey, LButton);
        end;
    end;
  end;
  grpManualStrategy.Width := LLeft;
end;



procedure TfrmMain.rgCommitSettingsClick(Sender: TObject);
begin
  g_AppSettings.CommitKeyInput := TCommitKeyInputType(rgCommitSettings.ItemIndex);
end;

procedure TfrmMain.seTimerTestIntervalChange(Sender: TObject);
begin
  try
    Timer2.Interval := seTimerTestInterval.Value;
    g_AppSettings.TestTimeInterval := seTimerTestInterval.Value;
  except
    //
  end;
end;

procedure TfrmMain.stat1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
begin
  case Panel.ID of
    0:
      begin
        StatusBar.Canvas.Font.Color := clBlack;
      end;
    1:
      begin
        if g_StrategyManager.IsReadyCommit then
        begin
          StatusBar.Canvas.Font.Color := clRed;
          StatusBar.Canvas.Font.Style := [fsBold];
        end
        else
        begin
          StatusBar.Canvas.Font.Color := clBlack;
          StatusBar.Canvas.Font.Style := [];
        end;
      end;

  end;
  // ��������
  TextOut(StatusBar.Canvas.Handle, Rect.Left, Rect.Top, PChar(Panel.Text),
    Length(Panel.Text));
end;

initialization;
  CriticalSection := TCriticalSection.Create;

finalization
  CriticalSection.Free;

end.
