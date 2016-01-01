object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #27169#25311#31383#20307
  ClientHeight = 281
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 91
    Width = 60
    Height = 13
    Caption = #24403#21069#20215#26684#65306
  end
  object Label2: TLabel
    Left = 98
    Top = 91
    Width = 40
    Height = 15
    Caption = '82600'
    Font.Charset = GB2312_CHARSET
    Font.Color = clRed
    Font.Height = -15
    Font.Name = #26032#23435#20307
    Font.Style = []
    ParentFont = False
  end
  object Button1: TButton
    Left = 240
    Top = 180
    Width = 121
    Height = 49
    Caption = #33719#21462#39564#35777#30721
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 88
    Top = 194
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object Button2: TButton
    Left = 240
    Top = 99
    Width = 121
    Height = 54
    Caption = #33258#23450#20041#21152#20215
    TabOrder = 2
    OnClick = Button2Click
  end
  object Edit2: TEdit
    Left = 88
    Top = 115
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '1000'
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 80
    Top = 248
  end
end
