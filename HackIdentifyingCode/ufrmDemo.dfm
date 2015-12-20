object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 374
  ClientWidth = 471
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 32
    Top = 24
    Width = 75
    Height = 25
    Caption = #36873#25321#25991#20214
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 120
    Top = 24
    Width = 75
    Height = 25
    Caption = #21152#36733#25991#20214
    TabOrder = 1
    OnClick = Button2Click
  end
  object chtRate: TChart
    Left = 32
    Top = 110
    Width = 400
    Height = 250
    Title.Text.Strings = (
      'TChart')
    Chart3DPercent = 1
    TabOrder = 2
    ColorPaletteIndex = 13
  end
  object btnCode0: TButton
    Left = 32
    Top = 79
    Width = 33
    Height = 25
    Caption = '0'
    TabOrder = 3
    OnClick = btnCode0Click
  end
  object btnCode1: TButton
    Left = 71
    Top = 79
    Width = 33
    Height = 25
    Caption = '0'
    TabOrder = 4
    OnClick = btnCode1Click
  end
  object btnCode2: TButton
    Left = 110
    Top = 79
    Width = 33
    Height = 25
    Caption = '0'
    TabOrder = 5
    OnClick = btnCode2Click
  end
  object btnCode3: TButton
    Left = 149
    Top = 79
    Width = 33
    Height = 25
    Caption = '0'
    TabOrder = 6
    OnClick = btnCode3Click
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.bmp'
    InitialDir = '..\Debug\Code'
    Left = 304
    Top = 24
  end
end
