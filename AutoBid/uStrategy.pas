unit uStrategy;

interface

uses
  Classes, SysUtils, Generics.Collections;


type
  TStrategyType = (stUnknown, stAutomatic, stManual);
  TBaseStrategy = class
  private
    FAddPrice: Integer;
    procedure SetAddPrice(const Value: Integer);
  protected
    function GetType: TStrategyType; virtual; abstract;
  public
    property AddPrice: Integer read FAddPrice write SetAddPrice;
    property Type_: TStrategyType read GetType;
  end;

  TAutomaticStrategy = class(TBaseStrategy)
  private
    FStartTime: TDateTime;
    FCommitTime: TDateTime;
    FCommitPrice: Integer;
    FCommitAddPrice: Integer;
    procedure SetStartTime(const Value: TDateTime);
    procedure SetCommitTime(const Value: TDateTime);
    procedure SetCommitPrice(const Value: Integer);
    procedure SetCommitAddPrice(const Value: Integer);
  protected
    function GetType: TStrategyType; override;
  public
    property StartTime: TDateTime read FStartTime write SetStartTime;
    property CommitTime: TDateTime read FCommitTime write SetCommitTime;
    property CommitAddPrice: Integer read FCommitAddPrice write SetCommitAddPrice;

    //实际提交价格，提交时填充
    property CommitPrice: Integer read FCommitPrice write SetCommitPrice;

  end;

  TManualStrategy = class(TBaseStrategy)
  private
    FHotKey: Integer;
    FCaption: string;
    procedure SetHotKey(const Value: Integer);
    procedure SetCaption(const Value: string);
  protected
    function GetType: TStrategyType; override;
  public
    property Caption :string read FCaption write SetCaption;
    property HotKey: Integer read FHotKey write SetHotKey;
  end;


  TAutoFillIndentifyCodeStatus = (aficsCompleted, aficsWaitFor);
  TStrategyManager = class(TList<TBaseStrategy>)
  private
    FIsReadyCommit: Boolean;
    FKeyInputInterval: Integer;
    FFileName: string;
    FIsCopyMode: Boolean;
    FAutoFillIndentifyCodeStatus: TAutoFillIndentifyCodeStatus;
    procedure ClearItems;
    procedure LoadFromFile;
    function CreateStrategy(AType: TStrategyType): TBaseStrategy;
    function BuildDateTime(const AString: string): TDateTime;
    procedure SetIsReadyCommit(const Value: Boolean);
    procedure SetKeyInputInterval(const Value: Integer);
    procedure SetIsCopyMode(const Value: Boolean);
  public
    procedure InvokeStrategy(AStrategy: TBaseStrategy);
    procedure Submit;
    procedure InputIndentifyCode(const ACode: string);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property IsReadyCommit: Boolean read FIsReadyCommit write SetIsReadyCommit;
    property IsCopyMode: Boolean read FIsCopyMode write SetIsCopyMode;
    property KeyInputInterval: Integer read FKeyInputInterval write SetKeyInputInterval;
  end;

  TAutoInvokeThread = class(TThread)
  private
    FStrategy: TAutomaticStrategy;
    procedure SetStrategy(const Value: TAutomaticStrategy);
  protected
    procedure Execute; override;
  public
    property Strategy: TAutomaticStrategy read FStrategy write SetStrategy;
  end;

var
  g_StrategyManager: TStrategyManager;


implementation

uses
  uPriceInfo, IniFiles, TypInfo, DateUtils, Forms, uHelp, USetting;

{ TManualStrategy }

function TManualStrategy.GetType: TStrategyType;
begin
  Result := stManual;
end;

procedure TManualStrategy.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TManualStrategy.SetHotKey(const Value: Integer);
begin
  FHotKey := Value;
end;

{ TAutomaticStrategy }

function TAutomaticStrategy.GetType: TStrategyType;
begin
  Result := stAutomatic;
end;



procedure TAutomaticStrategy.SetCommitAddPrice(const Value: Integer);
begin
  FCommitAddPrice := Value;
end;

procedure TAutomaticStrategy.SetCommitPrice(const Value: Integer);
begin
  FCommitPrice := Value;
end;

procedure TAutomaticStrategy.SetCommitTime(const Value: TDateTime);
begin
  FCommitTime := Value;
end;

procedure TAutomaticStrategy.SetStartTime(const Value: TDateTime);
begin
  FStartTime := Value;
end;

{ TBaseStrategy }


procedure TBaseStrategy.SetAddPrice(const Value: Integer);
begin
  FAddPrice := Value;
end;



{ TStrategyManager }

procedure TStrategyManager.AfterConstruction;
begin
  inherited;
  FAutoFillIndentifyCodeStatus := aficsCompleted;
  FFileName := ExtractFileDir(Application.ExeName) + '\Config.ini';
  LoadFromFile;
end;

procedure TStrategyManager.BeforeDestruction;
begin
  ClearItems;
  inherited;
end;



function TStrategyManager.BuildDateTime(const AString: string): TDateTime;
begin
  Result := Now;
  if Length(AString) = 8 then
  begin
    Result := EncodeDateTime(
            YearOf(Now),
            MonthOf(Now),
            DayOf(Now),
            StrToInt(Copy(AString,1,2)),
            StrToInt(Copy(AString,4,2)),
            StrToInt(Copy(AString,7,2)),
            0);
  end
  else if Length(AString) = 12 then
  begin
    Result := EncodeDateTime(
            YearOf(Now),
            MonthOf(Now),
            DayOf(Now),
            StrToInt(Copy(AString,1,2)),
            StrToInt(Copy(AString,4,2)),
            StrToInt(Copy(AString,7,2)),
            StrToInt(Copy(AString,10,3)));
  end;

end;

procedure TStrategyManager.ClearItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Self[I].Free;
  end;
  Clear;
end;

function TStrategyManager.CreateStrategy(AType: TStrategyType): TBaseStrategy;
begin
  Result := nil;
  case AType of
    stAutomatic: Result := TAutomaticStrategy.Create;
    stManual: Result := TManualStrategy.Create;
  end;
end;

procedure TStrategyManager.InputIndentifyCode(const ACode: string);
begin
  if FAutoFillIndentifyCodeStatus = aficsWaitFor then
  begin
    //选中验证码框
    MouseClick(g_ScreanPointSetting.GetCodePos);

    //输入验证码
    if FIsCopyMode then
      PasteInput(ACode)
    else
    begin
      ClearContent;
      KeyboardInput(ACode);
    end;
    Self.IsReadyCommit := True;
  end;
end;

procedure TStrategyManager.InvokeStrategy(AStrategy: TBaseStrategy);
//var
//  LPrice: Integer;
begin
  {
  //执行策略
  if AStrategy.FSmartAdjust then
    LPrice := g_PriceInfoManager.LastSmartAdjustPrice
  else
    LPrice := g_PriceInfoManager.LastPrice;
  LPrice := AStrategy.AddPrice + LPrice;

  g_PriceInfoManager.SetBidPrice(LPrice);
  //选中价格框
  MouseClick(g_ScreanPointSetting.GetPricePos);

  //输入价格
  if FIsCopyMode then
    PasteInput(IntToStr(LPrice))
  else
  begin
    ClearContent;
    KeyboardInput(IntToStr(LPrice));
  end;
  }

  //点自定义加价
  MouseClick(g_ScreanPointSetting.GetCustomAddPrice);
  //获取验证码
  MouseClick(g_ScreanPointSetting.GetBidPos);

//  if g_StrategyManager.AutoFillIndentifyCode then
//    FAutoFillIndentifyCodeStatus := aficsWaitFor;
end;

procedure TStrategyManager.LoadFromFile;
var
  LIniFile: TIniFile;
  LCount: Integer;
  I: Integer;
  LType: TStrategyType;
  LSection: string;
  LStrategy: TBaseStrategy;
  LTime: TDateTime;
  LTemp: string;
begin
  Assert(FileExists(FFileName), '找不到配置文件:Config.ini');
  LIniFile := TIniFile.Create(FFileName);
  LCount := LIniFile.ReadInteger('Settings', 'StrategyCount', 0);

  FKeyInputInterval := LIniFile.ReadInteger('Settings', 'KeyInputInterval', 100);
  g_KeyIntputInterval := FKeyInputInterval;
  FIsCopyMode :=
    StrToBoolDef(LIniFile.ReadString('Settings', 'IsCopyMode', ''),False);

  for I := 0 to LCount - 1 do
  begin
    LSection := 'Strategy' + IntToStr(I);
    LType := TStrategyType(GetEnumValue(TypeInfo(TStrategyType),
      ('st'+ LIniFile.ReadString(LSection, 'Type', 'unKnown'))));
    LStrategy := CreateStrategy(LType);
    Assert(Assigned(LStrategy), '策略类型配置错误');
    LStrategy.AddPrice := LIniFile.ReadInteger(LSection, 'AddPrice', 0);
    case LType of
      stAutomatic:
        begin
          LTemp := LIniFile.ReadString(LSection, 'StartTime', '');
          LTime := BuildDateTime(LTemp);
          TAutomaticStrategy(LStrategy).StartTime := LTime;

          LTemp := LIniFile.ReadString(LSection, 'CommitTime', '');
          LTime := BuildDateTime(LTemp);
          TAutomaticStrategy(LStrategy).CommitTime := LTime;

          TAutomaticStrategy(LStrategy).CommitAddPrice := LIniFile.ReadInteger(LSection, 'CommitAddPrice', 0);
        end;
      stManual:
        begin
          TManualStrategy(LStrategy).HotKey :=
            LIniFile.ReadInteger(LSection, 'HotKey', 0);
          TManualStrategy(LStrategy).Caption :=
            LIniFile.ReadString(LSection, 'Caption', '未命名');
        end;
    end;
    Self.Add(LStrategy);
  end;
  LIniFile.Free;
end;


procedure TStrategyManager.SetIsCopyMode(const Value: Boolean);
var
  LIniFile: TIniFile;
  LValue: string;
begin
  FIsCopyMode := Value;
  LIniFile := TIniFile.Create(FFileName);
  if FIsCopyMode then
    LValue := 'True'
  else
    LValue := 'False';
  LIniFile.WriteString('Settings', 'IsCopyMode', LValue);
  LIniFile.Free;
end;

procedure TStrategyManager.SetIsReadyCommit(const Value: Boolean);
begin
  FIsReadyCommit := Value;
end;

procedure TStrategyManager.SetKeyInputInterval(const Value: Integer);
var
  LIniFile: TIniFile;
begin
  FKeyInputInterval := Value;
  g_KeyIntputInterval := Value;
  LIniFile := TIniFile.Create(FFileName);
  LIniFile.WriteInteger('Settings', 'KeyInputInterval', FKeyInputInterval);
  LIniFile.Free;
end;

procedure TStrategyManager.Submit;
begin
  MouseClick(g_ScreanPointSetting.GetSubmitPos);
  FAutoFillIndentifyCodeStatus := aficsCompleted;
end;

{ TAutoInvokeThread }

procedure TAutoInvokeThread.Execute;
var
  LIsStart: Boolean;
  LIsCommit: Boolean;
begin
  inherited;
  FreeOnTerminate := True;
  LIsStart := False;
  LIsCommit := False;
  while not Assigned(FStrategy) or (not (LIsStart and LIsCommit)) do
  begin

    //允许本地时间为0
    {
    if g_PriceInfoManager.ServerTimeDelta = 0 then
      Continue;
    }
    if Assigned(FStrategy) then
    begin
      if not LIsStart and
       (FStrategy.StartTime <= Now) then
      begin
        LIsStart := True;
        if FStrategy.CommitAddPrice > 0 then
          FStrategy.CommitPrice := g_PriceInfoManager.LastPrice;
        g_StrategyManager.InvokeStrategy(FStrategy);
      end;

      if not LIsCommit and g_StrategyManager.IsReadyCommit and
       (FStrategy.CommitTime <= Now) then
      begin
        LIsCommit := True;
        g_StrategyManager.Submit;
      end;

      if not LIsCommit and g_StrategyManager.IsReadyCommit and
       (FStrategy.CommitAddPrice > 0) and (FStrategy.CommitPrice > 0) and
       (FStrategy.CommitAddPrice + FStrategy.CommitPrice - 300 <= g_PriceInfoManager.LastPrice) then
      begin
        LIsCommit := True;
        g_StrategyManager.Submit;
      end;
    end;
    Sleep(100);
  end;

end;

procedure TAutoInvokeThread.SetStrategy(const Value: TAutomaticStrategy);
begin
  FStrategy := Value;
end;

initialization;
  g_StrategyManager := TStrategyManager.Create;

finalization
  if Assigned(g_StrategyManager) then
    g_StrategyManager.Free;
end.
