object Form1: TForm1
  Left = 253
  Top = 102
  Width = 513
  Height = 355
  Caption = 'Sinchronization Sample'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object bnLoad: TButton
    Left = 24
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 0
    OnClick = bnLoadClick
  end
  object bnClose: TButton
    Left = 24
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Close'
    Enabled = False
    TabOrder = 1
    OnClick = bnCloseClick
  end
  object bnWait: TButton
    Left = 24
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Wait'
    Enabled = False
    TabOrder = 2
    OnClick = bnWaitClick
  end
  object Memo: TMemo
    Left = 112
    Top = 16
    Width = 385
    Height = 305
    TabOrder = 3
  end
  object bnState: TButton
    Left = 24
    Top = 136
    Width = 75
    Height = 25
    Caption = 'State'
    Enabled = False
    TabOrder = 4
    OnClick = bnStateClick
  end
  object bnDelay: TButton
    Left = 24
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Delay'
    Enabled = False
    TabOrder = 5
    OnClick = bnDelayClick
  end
  object Edit: TEdit
    Left = 8
    Top = 296
    Width = 97
    Height = 21
    TabOrder = 6
    Text = '-30000000'
  end
  object Timer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerTimer
    Left = 24
    Top = 224
  end
end
