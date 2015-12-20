unit uCutScreen;

interface

uses
  System.Classes, System.SysUtils, Winapi.Windows, IdHashMessageDigest,
  Vcl.Graphics, GdiPlus2009;

type
  TCutScreenMgr = Class
  private
    FFullScreenWidth: Integer;
    FFullScreenHeight: Integer;
    FFileName: string;
    FMD5: string;
    function CheckMD5(AStream: TStream): Boolean;
  public
    function GetCutScreenPrice: string;
    procedure InitMode(AFontName: string; AFontSize: Integer);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

var
  g_CutScreenMgr: TCutScreenMgr;

implementation

uses
  USetting, Vcl.Dialogs, uModeCompare, uColorUtil;



{ TCutScreenMgr }

procedure TCutScreenMgr.AfterConstruction;
begin
  inherited;
  FFullScreenWidth := GetSystemMetrics(SM_CXMAXTRACK);
  FFullScreenHeight := GetSystemMetrics(SM_CYMAXTRACK);
  FFileName := '.\code\tmp\CutScreen.bmp';
end;

procedure TCutScreenMgr.BeforeDestruction;
begin
  inherited;

end;

function TCutScreenMgr.CheckMD5(AStream: TStream): Boolean;
var
  LMD5: TIdHashMessageDigest5;
  LNewMD5String: string;
begin
  Result := False;
  if Assigned(AStream) then
  begin
    LMD5 := TIdHashMessageDigest5.Create;
    LNewMD5String := LMD5.HashStreamAsHex(AStream);
    if SameText(LNewMD5String, FMD5) then
      Result := True
    else
      FMD5 := LNewMD5String;
    LMD5.Free;
  end;
end;

function TCutScreenMgr.GetCutScreenPrice: string;
var
  LBitmap: TBitmap;
  LScreenDC: HDC;
  LRect: TRect;
  LStream: TMemoryStream;
  LGPBitmap: IGPBitmap;
begin
  Result := '';
  LRect := g_ScreanPointSetting.GetPriceDisplayRect;
  LBitmap := TBitmap.Create;
  LBitmap.Width := LRect.Width;
  LBitmap.Height := LRect.Height;
  LScreenDC := GetDC(0);
  BitBlt(
    LBitmap.Canvas.Handle,
    0,
    0,
    LRect.Width,
    LRect.Height,
    LScreenDC,
    LRect.Left,
    LRect.Top,
    SRCCOPY);
  ReleaseDC(0, LScreenDC);

  LStream := TMemoryStream.Create;
  LBitmap.SaveToStream(LStream);
  LStream.Position := 0;
  if not CheckMD5(LStream) then
  begin
    LGPBitmap := TGPBitmap.Create(LBitmap.Handle, GdipCreateHalftonePalette);
    //LGPBitmap.Save(FFileName, TGPImageFormat.Bmp);
    Result := g_FontModeCompareMgr.ScanMode(LGPBitmap);
  end;
  ;
  LStream.Free;



  LBitmap.Free;
end;


procedure TCutScreenMgr.InitMode(AFontName: string; AFontSize: Integer);
begin
//
end;

initialization
  if not Assigned(g_CutScreenMgr) then
    g_CutScreenMgr := TCutScreenMgr.Create;

finalization
  g_CutScreenMgr.Free;

end.
