unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.jpeg,
  Vcl.ExtCtrls, uOwnerDrawPanel;

type
  TPricePanel = class(TOwnerDrawPanel)
  protected
    procedure OnPaint(ADC: HDC); override;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Button2: TButton;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FPricePanel: TPricePanel;

    { Private declarations }
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Unit2, System.DateUtils, GdiPlus2009;

{$R *.dfm}

procedure TForm1.AfterConstruction;
begin
  inherited;
  FPricePanel := TPricePanel.Create(Self);
  FPricePanel.Parent := Self;
  FPricePanel.Left := 100;
  FPricePanel.Top := 50;
  FPricePanel.Width := 100;
  FPricePanel.Height := 30;
  FPricePanel.Caption := '82600';
end;

procedure TForm1.BeforeDestruction;
begin
  FPricePanel.Free;
  inherited;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  LLfrm: TForm2;
begin
  LLfrm := TForm2.Create(Self);
  LLfrm.Left := self.Left + 200;
  LLfrm.Top := self.Top + 200;

  LLfrm.ShowModal;
  LLfrm.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Edit1.Text := IntToStr(StrToIntDef(Edit2.Text, 0) +
    StrToIntDef(Label2.Caption, 0));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  self.Left := 500;
  self.Top := 300;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  LString: string;
begin
  LString := IntToStr(80000 + SecondOf(Now) * 100);
  Label2.Caption := LString;
  FPricePanel.Caption := LString;
  FPricePanel.Refresh;
end;

{ TPricePanel }

procedure TPricePanel.OnPaint(ADC: HDC);
var
  LGraphics: IGPGraphics;
  LRect: TGPRect;
  LBrush: IGPBrush;
  LFont: IGPFont;
  LPt: TGPPointF;
begin
  LGraphics := TGPGraphics.Create(ADC);

  LRect := TGPRect.Create(0, 0, Width, Height);
  LBrush := TGPSolidBrush.Create(TGPColor.CreateFromColorRef(clBtnFace));
  LGraphics.FillRectangle(LBrush, LRect);

  LFont := TGPFont.Create('ËÎÌו',14,[],UnitPixel);
  LBrush := TGPSolidBrush.Create(TGPColor.CreateFromColorRef(clRed));
  LPt := TGPPointF.Create(0,0);
  LGraphics.DrawString(Self.Caption, LFont, LPt,
    TGPStringFormat.GenericDefault, LBrush);
end;

end.

{ TPricePanel }


