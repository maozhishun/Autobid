unit uOwnerDrawPanel;

interface


uses
  Classes, SysUtils, Controls, Windows, Messages, Graphics,
  GdiPlus2009;

type
  TPanelState = (psNormal, psHover, psFocus, psDown);
  TPanelStates = set of TPanelState;
  TOwnerDrawPanel = class(TCustomControl)
  protected
    procedure WMEraseBkgnd( var Msg: TWMEraseBkgnd); message WM_EraseBkgnd;
    procedure CMMouseEnter( var Msg: TMessage ); message CM_MOUSEENTER;
    procedure CMMouseLeave( var Msg: TMessage ); message CM_MouseLeave;
  protected
    FState: TPanelState;
    FTransparent: Boolean;
    FEnableStates: TPanelStates;
    procedure SetState(const Value: TPanelState);
    procedure SetTransparent(const Value: Boolean);
    procedure SetEnableStates(const Value: TPanelStates);

    procedure OnPaint(ADC: HDC); virtual;
    procedure WMPaint(var AMsg: TMessage); message WM_PAINT;
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager;
    property DockSite;
    property DoubleBuffered default False;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentFont default False;
    property Font;
    property Height;
    property Visible;
    property Width;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnGesture;
    property OnGetSiteInfo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnUnDock;
  public
    procedure AfterConstruction; override;

    property State: TPanelState read FState write SetState;
    property EnableStates: TPanelStates read FEnableStates write SetEnableStates;
    property Transparent: Boolean read FTransparent write SetTransparent;
  end;

implementation

procedure TOwnerDrawPanel.AfterConstruction;
begin
  inherited;
  FTransparent := True;
  ParentBackground := False;
  State := psNormal;
  FEnableStates := [psNormal];
  OnMouseDown := DoMouseDown;
  OnMouseUp := DoMouseUp;
end;

procedure TOwnerDrawPanel.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  if (Msg.LParam = 0) then
  begin
    State := psHover;
    if FState in FEnableStates then
      Refresh;
  end;
end;

procedure TOwnerDrawPanel.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if (Msg.LParam = 0) then
  begin
    State := psNormal;
    if FState in FEnableStates then
      Refresh;
  end;
end;

procedure TOwnerDrawPanel.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  State := psDown;
  if FState in FEnableStates then
    Refresh;
end;

procedure TOwnerDrawPanel.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  State := psHover;
  if FState in FEnableStates then
    Refresh;
end;

procedure TOwnerDrawPanel.OnPaint(ADC: HDC);
begin
//
end;

procedure TOwnerDrawPanel.SetEnableStates(const Value: TPanelStates);
begin
  FEnableStates := Value;
end;

procedure TOwnerDrawPanel.SetState(const Value: TPanelState);
begin
  FState := Value;
end;

procedure TOwnerDrawPanel.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
end;

procedure TOwnerDrawPanel.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  if FTransparent then
  begin
    Msg.Result := 1;
  end
  else
    inherited;
end;

procedure TOwnerDrawPanel.WMPaint(var AMsg: TMessage);
var
  LPs: TPaintStruct;
  LDC: HDC;
  LBufferBMP: TBitmap;//自己建立双缓冲
  LCanvas: TCanvas;
begin
  LBufferBMP := TBitmap.Create;
  LBufferBMP.Width := Self.Width;
  LBufferBMP.Height := Self.Height;
  OnPaint(LBufferBMP.Canvas.Handle);
  LDC := BeginPaint(Handle, LPs);
  LCanvas := TCanvas.Create;
  LCanvas.Handle := LDC;
  LCanvas.Draw(0, 0, LBufferBMP);
  EndPaint(Handle, LPs);
  LCanvas.Handle := 0;
  LCanvas.Free;
  LBufferBMP.Free;
end;

end.
