unit uModeCompare;

interface

uses
  System.Classes, System.SysUtils, Winapi.Windows,
  GdiPlus2009, System.Generics.Collections, Vcl.Graphics;


type
  TModeItem = class
  private
    FCode: string;
    FBitmap: IGPBitmap;
    procedure SetCode(const Value: string);
    procedure SetBitmap(const Value: IGPBitmap);
  public
    property Bitmap: IGPBitmap read FBitmap write SetBitmap;
    property Code: string read FCode write SetCode;
  end;


  TModeRates = array of Word;
  TModeCompareMgr = class
  protected
    FModeList: TList<TModeItem>;
    function CalcSamePixelCount(ASource, AMode: IGPBitmap;
      ASourceHDelta: Integer = 0; ASourceVDelta: Integer = 0): Integer;
    function CalcSamePixelRate(ASource, AMode: IGPBitmap;
      ASourceHDelta: Integer = 0; ASourceVDelta: Integer = 0): Integer;
    procedure InitMode; virtual;
    procedure UnInitMode; virtual;
  public
    function CompareMode(ASource: IGPBitmap;var ARates: TModeRates): string;
    function CompareModeRate(ASource: IGPBitmap;var ARates: TModeRates): string;
    function IsBlank(ASource: IGPBitmap): Boolean;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TFontModeCompareMgr = class(TModeCompareMgr)
  private
    FFileName: string;
    FFontName: string;
    FFontSize: Integer;
    FFontStyles: TFontStyles;
    function CreateFontBitmap(AInt: Integer): IGPBitmap;
    function TrimBitmap(ASource: IGPBitmap): IGPBitmap;

    function FontStylesToInt(AFontStyles: TFontStyles): Integer;
    function IntToFontStyles(AInt: Integer): TFontStyles;
    procedure LoadFromFile;
    procedure SaveToFile;
  protected
    procedure InitMode; override;
  public
    procedure SetFontMode(AFontName: string; AFontSize: Integer; AFontStyles: TFontStyles);
    procedure AfterConstruction; override;
    function ScanMode(ASource: IGPBitmap): string;
    property FontName: string read FFontName;
    property FontSize: Integer read FFontSize;
    property FontStyles: TFontStyles read FFontStyles;
  end;

var
  g_ModeCompareMgr: TModeCompareMgr;
  g_FontModeCompareMgr: TFontModeCompareMgr;

implementation

uses
  Vcl.Dialogs, uColorUtil, System.IniFiles, Vcl.Forms;

{ TModeItem }

procedure TModeItem.SetBitmap(const Value: IGPBitmap);
begin
  FBitmap := Value;
end;

procedure TModeItem.SetCode(const Value: string);
begin
  FCode := Value;
end;

{ TModeCompareMgr }

procedure TModeCompareMgr.AfterConstruction;
begin
  inherited;
  FModeList := TList<TModeItem>.Create;
  InitMode;
end;

procedure TModeCompareMgr.BeforeDestruction;
begin
  UnInitMode;
  FModeList.Free;
  inherited;
end;

function TModeCompareMgr.CalcSamePixelCount(ASource, AMode: IGPBitmap;
  ASourceHDelta: Integer = 0; ASourceVDelta: Integer = 0): Integer;
var
  I, J: Integer;
begin
  Result := 0;
  { TODO : 拉伸到一样大小 }
  Assert((ASource.Width >= AMode.Width + ASourceHDelta) and
    (ASource.Height >= AMode.Height + ASourceVDelta), '图片大小不一样');
  for I := 0 to AMode.Width - 1 do
    for J := 0 to AMode.Height - 1 do
      if ASource.Pixels[I + ASourceHDelta,J + ASourceVDelta].ColorRef =
        AMode.Pixels[I,J].ColorRef then
        Inc(Result);
end;

function TModeCompareMgr.CalcSamePixelRate(ASource, AMode: IGPBitmap;
  ASourceHDelta: Integer = 0; ASourceVDelta: Integer = 0): Integer;
var
  I, J: Integer;
  LCount: Integer;
  LSame: Integer;
begin
  Result := 0;
  LCount := 0;
  LSame := 0;
  { TODO : 拉伸到一样大小 }
  Assert((ASource.Width >= AMode.Width + ASourceHDelta) and
    (ASource.Height >= AMode.Height + ASourceVDelta), '图片大小不一样');
  for I := 0 to AMode.Width - 1 do
    for J := 0 to AMode.Height - 1 do
    begin
      if (AMode.Pixels[I,J].ColorRef = clBlack) or
        (ASource.Pixels[I + ASourceHDelta, J + ASourceVDelta].ColorRef = clBlack) then
      begin
        Inc(LCount);
        if ASource.Pixels[I + ASourceHDelta, J + ASourceVDelta].ColorRef =
          AMode.Pixels[I,J].ColorRef then
          Inc(LSame);
      end;
    end;
  if LCount > 0 then
    Result := Round(100 * LSame div LCount);
end;

function TModeCompareMgr.CompareMode(ASource: IGPBitmap; var ARates: TModeRates): string;
var
  I: Integer;
  LMaxCount: Integer;
  LCount: Integer;
begin
  Result := '';
  SetLength(ARates, FModeList.Count);
  LMaxCount := 0;
  for I := 0 to FModeList.Count - 1 do
  begin
    LCount := CalcSamePixelCount(ASource, FModeList[I].Bitmap);
    if LCount > LMaxCount then
    begin
      LMaxCount := LCount;
      Result := FModeList[I].Code;
    end;
    ARates[I] := Round(100 * LCount div Integer(ASource.Width * ASource.Height));
  end;
end;

function TModeCompareMgr.CompareModeRate(ASource: IGPBitmap;
  var ARates: TModeRates): string;
var
  I: Integer;
  LMaxRate: Integer;
  LRate: Integer;
begin
  Result := '';
  SetLength(ARates, FModeList.Count);
  LMaxRate := 0;
  for I := 0 to FModeList.Count - 1 do
  begin
    LRate := CalcSamePixelRate(ASource, FModeList[I].Bitmap);
    if LRate > LMaxRate then
    begin
      LMaxRate := LRate;
      Result := FModeList[I].Code;
    end;
    ARates[I] := LRate;
  end;
end;

procedure TModeCompareMgr.InitMode;
var
  I: Integer;
  LItem: TModeItem;
begin
  for I := 0 to 9 do
  begin
    LItem := TModeItem.Create;
    LItem.Code := IntToStr(I);
    LItem.Bitmap := TGPBitmap.Create(Format('.\code\mode\_%d.bmp', [I]));;
    FModeList.Add(LItem);
  end;
end;

function TModeCompareMgr.IsBlank(ASource: IGPBitmap): Boolean;
var
  I, J: Integer;
  LCount: Integer;
begin
  LCount := 0;
  for I := 0 to ASource.Width - 1 do
    for J := 0 to ASource.Height - 1 do
      if ASource.Pixels[I,J].ColorRef = 0 then
      begin
        Inc(LCount);
      end;
  Result := LCount < 20;
end;

procedure TModeCompareMgr.UnInitMode;
var
  I: Integer;
begin
  for I := 0 to FModeList.Count - 1 do
  begin
    FModeList[I].Free;
  end;
  FModeList.Clear;
end;

{ TFontModeCompareMgr }

procedure TFontModeCompareMgr.AfterConstruction;
begin
  FFileName := ExtractFileDir(Application.ExeName) + '\FontMode.ini';
  FFontName := '宋体';
  FFontSize := 11;
  FFontStyles := [];
  LoadFromFile;
  inherited;

end;

function TFontModeCompareMgr.CreateFontBitmap(AInt: Integer): IGPBitmap;
var
  LBitmap: TBitmap;
  LRect: TRect;
  LGpBitmap: IGPBitmap;
begin
  Result := nil;
  if (AInt >= 0) and (AInt <= 9) then
  begin
    LBitmap := TBitmap.Create;
    LBitmap.Canvas.Font.Name := FFontName;
    LBitmap.Canvas.Font.Size := FFontSize;
    LBitmap.Canvas.Font.Style := FFontStyles;
    LBitmap.Canvas.Font.Color := clBlack;
    LBitmap.Width := LBitmap.Canvas.TextWidth(IntToStr(AInt));
    LBitmap.Height := LBitmap.Canvas.TextHeight(IntToStr(AInt));
    LRect.Left := 0;
    LRect.Top := 0;
    LRect.Width := LBitmap.Width;
    LRect.Height := LBitmap.Height;

    DrawText(
      LBitmap.Canvas.Handle,
      IntToStr(AInt),
      -1,
      LRect,
      DT_LEFT + DT_TOP
      );
    LGpBitmap := TGPBitmap.Create(LBitmap.Handle, GdipCreateHalftonePalette);

    Result := TrimBitmap(LGpBitmap);
    LBitmap.Free
  end;
end;

function TFontModeCompareMgr.FontStylesToInt(AFontStyles: TFontStyles): Integer;
begin
  Result := 0;
  if fsBold in AFontStyles then
    Result := Result + 1;
  if fsItalic in AFontStyles then
    Result := Result + 2;
  if fsUnderline in AFontStyles then
    Result := Result + 4;
  if fsStrikeOut in AFontStyles then
    Result := Result + 8;
end;

procedure TFontModeCompareMgr.InitMode;
var
  I: Integer;
  LItem: TModeItem;
begin
  for I := 0 to 9 do
  begin
    LItem := TModeItem.Create;
    LItem.Code := IntToStr(I);
    LItem.Bitmap := CreateFontBitmap(I);
    LItem.Bitmap.Save(Format('.\code\modeFont\_%d.bmp', [I]), TGPImageFormat.Bmp);
    FModeList.Add(LItem);
  end;
end;


function TFontModeCompareMgr.IntToFontStyles(AInt: Integer): TFontStyles;
begin
  Result := [];
  if AInt and 1 = 1 then
    Result := Result + [fsBold];
  if AInt and 2 = 2 then
    Result := Result + [fsItalic];
  if AInt and 4 = 4 then
    Result := Result + [fsUnderline];
  if AInt and 8 = 8 then
    Result := Result + [fsStrikeOut];
end;

procedure TFontModeCompareMgr.LoadFromFile;
var
  LIniFile: TIniFile;
begin
  if FileExists(FFileName) then
  begin
    LIniFile := TIniFile.Create(FFileName);
    FFontName :=
      LIniFile.ReadString('Values', 'FontName', FFontName);
    FFontSize :=
      LIniFile.ReadInteger('Values', 'FontSize', FFontSize);
    FFontStyles :=
      IntToFontStyles(LIniFile.ReadInteger('Values', 'FontStyles', FontStylesToInt(FFontStyles)));
    LIniFile.Free;
  end;
end;

function ResultCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  LInt1: Integer;
  LInt2: Integer;
begin
  LInt1 := StrToIntDef(List.Names[Index1], 0);
  LInt2 := StrToIntDef(List.Names[Index2], 0);
  if LInt1 = LInt2 then
    Result := 0
  else if LInt1 > LInt2 then
    Result := 1
  else if LInt1 < LInt2 then
    Result := -1
  else
    Result := 0;

end;

procedure TFontModeCompareMgr.SaveToFile;
var
  LIniFile: TIniFile;
begin
  LIniFile := TIniFile.Create(FFileName);
  LIniFile.WriteString('Values', 'FontName', FFontName);
  LIniFile.WriteInteger('Values', 'FontSize', FFontSize);
  LIniFile.WriteInteger('Values', 'FontStyles', FontStylesToInt(FFontStyles));
  LIniFile.Free;
end;

function TFontModeCompareMgr.ScanMode(ASource: IGPBitmap): string;
var
  I, J, K: Integer;
  LModeItem: TModeItem;
  LModeBitmap: IGPBitmap;

  LTemp: IGPBitmap;
  LCount: Integer;
  LResultList: TStringList;
begin
  Result := '';
  if Assigned(ASource) then
  begin
    LTemp := ASource.Clone;
    LTemp := TrimBitmap(LTemp);
    if Assigned(LTemp) then
    begin
      LTemp.Save('.\code\tmp\CutScreen.bmp', TGPImageFormat.Bmp);

      //纵向扫描
      LResultList := TStringList.Create;
      for I := 0 to LTemp.Height - 1 do
      begin
        //0-9循环
        for J := 0 to 9 do
        begin
          LModeItem := FModeList[J];
          LModeBitmap := LModeItem.Bitmap;

          if I + LModeBitmap.Height > LTemp.Height then
            Continue;

          //横向扫描
          for K := 0 to LTemp.Width - LModeBitmap.Width do
          begin
            LCount := CalcSamePixelCount(LTemp, LModeBitmap, K, I);
            if LCount = LModeBitmap.Width * LModeBitmap.Height then
            begin
              LResultList.Add(IntToStr(K) + '=' + LModeItem.FCode);
            end;
          end;
        end;
        if LResultList.Count > 0 then
          Break;
      end;


      LResultList.CustomSort(ResultCompare);
      for I := 0 to LResultList.Count - 1 do
        Result := Result + LResultList.ValueFromIndex[I];

      LResultList.Free;
    end;
  end;

end;

procedure TFontModeCompareMgr.SetFontMode(AFontName: string;
  AFontSize: Integer; AFontStyles: TFontStyles);
begin
  FFontName := AFontName;
  FFontSize := AFontSize;
  FFontStyles := AFontStyles;
  UnInitMode;
  InitMode;
  SaveToFile;
end;

function TFontModeCompareMgr.TrimBitmap(ASource: IGPBitmap): IGPBitmap;
var
  I, J: Integer;
  LFound: Boolean;
  LGpBitmap: IGPBitmap;
  LRect: TRect;
  LTrimBMP: TBitmap;
begin
  Result := nil;
  LGpBitmap := ASource.Clone;
  GraySplit(LGpBitmap, PriceGrayMin, PriceGrayMax);
  //裁剪留白区域
  //Left
  for I := 0 to LGpBitmap.Width - 1 do
  begin
    LFound := False;
    for J := 0 to LGpBitmap.Height - 1 do
    begin
      if LGpBitmap.Pixels[I, J].ColorRef = clBlack then
      begin
        LRect.Left := I;
        LFound := True;
        Break;
      end;
    end;
    if LFound then
      Break;
  end;

  //Top
  for I := 0 to LGpBitmap.Height - 1 do
  begin
    LFound := False;
    for J := 0 to LGpBitmap.Width - 1 do
    begin
      if LGpBitmap.Pixels[J, I].ColorRef = clBlack then
      begin
        LRect.Top := I;
        LFound := True;
        Break;
      end;
    end;
    if LFound then
      Break;
  end;

  //Right
  for I := LGpBitmap.Width - 1 downto 0 do
  begin
    LFound := False;
    for J := 0 to LGpBitmap.Height - 1 do
    begin
      if LGpBitmap.Pixels[I, J].ColorRef = clBlack then
      begin
        LRect.Right := I;
        LFound := True;
        Break;
      end;
    end;
    if LFound then
      Break;
  end;


  //Bottom
  for I := LGpBitmap.Height - 1 downto 0 do
  begin
    LFound := False;
    for J := 0 to LGpBitmap.Width - 1 do
    begin
      if LGpBitmap.Pixels[J, I].ColorRef = clBlack then
      begin
        LRect.Bottom := I;
        LFound := True;
        Break;
      end;
    end;
    if LFound then
      Break;
  end;


  if (LRect.Width > 0) and (LRect.Height > 0) then
  begin
    LTrimBMP := TBitmap.Create;
    LTrimBMP.Width := LRect.Width + 1;
    LTrimBMP.Height := LRect.Height + 1;
    Result := TGPBitmap.Create(LTrimBMP.Handle, GdipCreateHalftonePalette);
    for I := 0 to Result.Width - 1 do
      for J := 0 to Result.Height - 1 do
      begin
        Result.Pixels[I, J] := TGPColor.CreateFromColorRef(
          LGpBitmap.Pixels[I + LRect.Left, J + LRect.Top].ColorRef)
      end;
    LTrimBMP.Free;
  end;
end;

initialization
  g_ModeCompareMgr := TModeCompareMgr.Create;
  g_FontModeCompareMgr := TFontModeCompareMgr.Create;


finalization
  if Assigned(g_ModeCompareMgr) then
    g_ModeCompareMgr.Free;
  if Assigned(g_FontModeCompareMgr) then
    g_FontModeCompareMgr.Free;

end.
