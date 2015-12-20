unit ufrmDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, GdiPlus2009, uModeCompare,
  VCLTee.TeEngine, Vcl.ExtCtrls, VCLTee.TeeProcs, VCLTee.Chart, VCLTee.Series,
  uIndentifyingCodeDefine;

type
  TForm1 = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    Button2: TButton;
    chtRate: TChart;
    btnCode0: TButton;
    btnCode1: TButton;
    btnCode2: TButton;
    btnCode3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnCode0Click(Sender: TObject);
    procedure btnCode1Click(Sender: TObject);
    procedure btnCode2Click(Sender: TObject);
    procedure btnCode3Click(Sender: TObject);
  private
    FTestImage: IGPBitmap;
    FRecord: PHackResultRecord;
    procedure GetCodeByURLCallBack(const AURL: PAnsiChar; AURLLength: Integer;
    const ACode: PAnsiChar; ACodeLength: Integer; ASuccess: Boolean);
    procedure LoadRate(AIndex: Integer);
    { Private declarations }
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;


    { Public declarations }
  end;


  function GetCodeByFile(const AFileName: PAnsiChar; ALength: Integer;
    var AOutLength: Integer): PAnsiChar; stdcall; external 'IndentifyingCodeKiller.dll';


  function GetCodeByFileWithRecord(const AFileName: PAnsiChar; ALength: Integer;
    var AOutLength: Integer; ARecord: Pointer): PAnsiChar; stdcall; external 'IndentifyingCodeKiller.dll';

  function GetCodeByURL(const AURL: PAnsiChar; AURLLength: Integer;
    ACallBack: TGetCodeByURLCallBack;
    ARetryTimes: Word = 3; ATimeOut: Cardinal = 500): Boolean; stdcall; external 'IndentifyingCodeKiller.dll';


var
  Form1: TForm1;

implementation

uses
  uColorUtil, Winapi.ActiveX, Winapi.UrlMon;

{$R *.dfm}

{ TForm1 }

procedure TForm1.AfterConstruction;
begin
  inherited;
  New(FRecord);


end;



procedure TForm1.BeforeDestruction;
begin
  Dispose(FRecord);
  inherited;
end;

procedure TForm1.btnCode0Click(Sender: TObject);
begin
  LoadRate(0);
end;

procedure TForm1.btnCode1Click(Sender: TObject);
begin
  LoadRate(1);
end;

procedure TForm1.btnCode2Click(Sender: TObject);
begin
  LoadRate(2);
end;

procedure TForm1.btnCode3Click(Sender: TObject);
begin
  LoadRate(3);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  LFileName: string;
  LAnsiFileName: AnsiString;
  LLength, LOutLength: Integer;
  LResultChar: PAnsiChar;
  LResultString: AnsiString;
  LReulst: string;
begin
  if OpenDialog1.Execute(self.Handle) then
    LFileName := OpenDialog1.FileName;

  if not FileExists(LFileName) then
    Exit;

  LAnsiFileName := LFileName;
  LLength := Length(LAnsiFileName);
  LResultChar := GetCodeByFileWithRecord(PAnsiChar(LAnsiFileName), LLength, LOutLength, FRecord);
  SetLength(LResultString, LOutLength);
  LResultString := LResultChar;
  LReulst := string(LResultString);
  if Length(LReulst) = 4 then
  begin
    btnCode0.Caption := LReulst[1];
    btnCode1.Caption := LReulst[2];
    btnCode2.Caption := LReulst[3];
    btnCode3.Caption := LReulst[4];
  end;
  ShowMessage(LReulst);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  LAnsiURL: AnsiString;
begin
  LAnsiURL := 'http://bu1tpub:8082/autobid/0.jpg';
  GetCodeByURL(PAnsiChar(LAnsiURL), Length(LAnsiURL), GetCodeByURLCallBack, 3, 500);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
//  LoadRate;
end;

procedure TForm1.GetCodeByURLCallBack(const AURL: PAnsiChar; AURLLength: Integer;
    const ACode: PAnsiChar; ACodeLength: Integer; ASuccess: Boolean);
var
  LAnsiURL, LAnsiCode: AnsiString;
begin
  SetLength(LAnsiURL, AURLLength);
  SetLength(LAnsiCode, ACodeLength);
  LAnsiURL := AURL;
  LAnsiCode := ACode;

  ShowMessage('URL:'+ string(LAnsiURL) + #13#10 + 'Code:' + string(LAnsiCode) + #13#10 + 'Success:' +
    BoolToStr(ASuccess));
end;

procedure TForm1.LoadRate(AIndex: Integer);
var
  LSeries: TBarSeries;
  I: Integer;
  LMaxValue: Integer;
  LMaxIndex: Integer;
begin
  chtRate.ClearChart;

  LSeries := TBarSeries.Create(chtRate);
  LSeries.ParentChart := chtRate;


  LMaxIndex := -1;
  LMaxValue := -1;

  for I := 0 to 9 do
  begin
    if FRecord.Rates[AIndex, I] > LMaxValue then
    begin
      LMaxValue := FRecord.Rates[AIndex, I];
      LMaxIndex := I;
    end;
  end;


  for I := 0 to 9 do
  begin
    if LMaxIndex = I then
      LSeries.AddXY(I, FRecord.Rates[AIndex, I], '', $00317DED)
    else
      LSeries.AddXY(I, FRecord.Rates[AIndex, I], '', $00D59B5B);
  end;

  chtRate.AddSeries(LSeries);
  chtRate.Legend.Visible := False;
  chtRate.Chart3DPercent := 1;
end;

end.


