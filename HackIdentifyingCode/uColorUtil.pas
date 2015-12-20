unit uColorUtil;

interface

uses
  System.Classes, System.SysUtils, GdiPlus2009, Vcl.Controls, Vcl.Graphics,
  uIndentifyingCodeDefine;



  //计算灰度值
  function GetGrayNum(R, G, B: Cardinal): Byte;
  //灰度分割
  function GraySplit(ABitmap: IGPBitmap; AGrayMin, AGrayMax: Byte): Boolean;

  //获取验证码
  function GetCodeByFile(const AFileName: PAnsiChar; ALength: Integer;
    var AOutLength: Integer): PAnsiChar; stdcall;

  //获取验证码,附加比对信息
  function GetCodeByFileWithRecord(const AFileName: PAnsiChar; ALength: Integer;
    var AOutLength: Integer; ARecord: Pointer): PAnsiChar; stdcall;

  function GetCode(const AFileName: string; ARecord: PHackResultRecord = nil): string;

const
   PriceGrayMin = 0;
   PriceGrayMax = 220;
exports
  GetCodeByFile,
  GetCodeByFileWithRecord;


implementation

uses
  uModeCompare, Vcl.Dialogs;

function GetCodeByFile(const AFileName: PAnsiChar; ALength: Integer;
    var AOutLength: Integer): PAnsiChar;
var
  LAnsiString: AnsiString;
  LResultString: AnsiString;
begin
  SetLength(LAnsiString, ALength);
  LAnsiString := AFileName;
  LResultString := AnsiString(GetCode(string(AFileName)));
  AOutLength := Length(LResultString);
  Result := PAnsiChar(LResultString);
end;

function GetCodeByFileWithRecord(const AFileName: PAnsiChar; ALength: Integer;
    var AOutLength: Integer; ARecord: Pointer): PAnsiChar; stdcall;
var
  LAnsiString: AnsiString;
  LResultString: AnsiString;
begin
  SetLength(LAnsiString, ALength);
  LAnsiString := AFileName;
  LResultString := AnsiString(GetCode(string(AFileName), PHackResultRecord(ARecord)));
  AOutLength := Length(LResultString);
  Result := PAnsiChar(LResultString);
end;

function GetCode(const AFileName: string; ARecord: PHackResultRecord = nil): string;
const
  single_top=6;
  single_height=19;
  single_width=17;
  left_indent=5;
  mid_indent=-1;
//  left_indent=2;
//  mid_indent=1;
var
  LBufferBMP: TBitmap;
  I, J, K, M: Integer;
  LSingleImage: IGPBitmap;
  LTestImage: IGPBitmap;
  LBeginPos: Integer;
  LGraphics: IGPGraphics;
  LGPRect: TGPRectF;
  LResultCode: string;
  LFileName: string;
  LRates: TModeRates;
  LCodeIndex: Integer;
begin
  Result := '';

  if not FileExists(AFileName) then
    Exit;

  LTestImage := TGPBitmap.Create(AFileName);

  if GraySplit(LTestImage, 40, 85) then
  begin
    {$IFDEF DEBUG}
    LTestImage.Save('.\code\tmp\GraySplit.bmp', TGPImageFormat.Bmp);
    {$ENDIF}

    LBeginPos := left_indent;
    for I := 0 to 5 do
    begin
      LBufferBMP := TBitmap.Create;
      LBufferBMP.Width := single_width;
      LBufferBMP.Height := single_height;
      LSingleImage := TGPBitmap.Create(LBufferBMP.Handle, GdipCreateHalftonePalette);
      for J := 0 to single_width - 1 do
        for K := 0 to single_height - 1 do
        begin
          LSingleImage.Pixels[J, K] := TGPColor.CreateFromColorRef(
            LTestImage.Pixels[J + LBeginPos, K + single_top].ColorRef)
        end;
      {$IFDEF DEBUG}
      LSingleImage.Save(Format('.\code\tmp\%d.bmp', [I]), TGPImageFormat.Bmp);
      {$ENDIF}
      if not g_ModeCompareMgr.IsBlank(LSingleImage) then
      begin
        LResultCode := LResultCode + g_ModeCompareMgr.CompareModeRate(LSingleImage, LRates);
        if Assigned(ARecord) then
        begin
          LCodeIndex := Length(LResultCode);
          for M := 0 to Length(LRates) - 1 do
          begin
            ARecord.Rates[LCodeIndex - 1, M] := LRates[M];
          end;
        end;
          
      end;
      LBeginPos := LBeginPos + single_width + mid_indent;
      LBufferBMP.Free;
    end;
    Result := LResultCode;
  end;
end;

function GetGrayNum(R, G, B: Cardinal): Byte;
begin
  // 灰度 = (蓝色分量 * 11 + 绿色分量 * 59 + 红色分量 * 30) / 100;
  //Result  := (B * 搜索128 + G * 512 + R * 256) div 1024;
  Result  := (B * 11 + G * 59 + R * 30) div 100;
end;

function GraySplit(ABitmap: IGPBitmap; AGrayMin, AGrayMax: Byte): Boolean;
var
  I, J: Integer;
  LWidth: Integer;
  LHeight: Integer;
  LGray: Byte;
  LGrayTable: array of Integer;
  LColor: TGpColor;
  LStrings: TStringList;
begin
  Result := False;
  if Assigned(ABitmap) then
  begin
    LWidth := ABitmap.Width;
    LHeight := ABitmap.Height;
    SetLength(LGrayTable, 256);

    //计算灰度值
    for I := 0 to LWidth - 1 do
    begin
      for J := 0 to LHeight - 1 do
      begin
        LColor := ABitmap.Pixels[I, J];
        LGray := GetGrayNum(LColor.R, LColor.G, LColor.B);

        Inc(LGrayTable[LGray]);

        if (LGray <= AGrayMax) and (LGray >= AGrayMin) then
          ABitmap.Pixels[I, J] := TGPColor.CreateFromColorRef(clBlack)
        else
          ABitmap.Pixels[I, J] := TGPColor.CreateFromColorRef(clWhite);

      end;
    end;

    //输出灰度表
    LStrings := TStringList.Create;
    for I := 0 to Length(LGrayTable) - 1 do
      LStrings.Add(IntToStr(LGrayTable[I]));
//    LStrings.SaveToFile('D:\GrayTable.txt');
    LStrings.Free;
    Result := True;
  end;

end;

end.
