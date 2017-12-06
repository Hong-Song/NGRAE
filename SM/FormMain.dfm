object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 495
  ClientWidth = 635
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
  object pnDB: TPanel
    Left = 8
    Top = 8
    Width = 569
    Height = 161
    TabOrder = 0
  end
  object Button1: TButton
    Left = 248
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
  end
  object rgConnectType: TRadioGroup
    Left = 24
    Top = 175
    Width = 185
    Height = 105
    Caption = 'Connection Type'
    ItemIndex = 0
    Items.Strings = (
      'ADOConnection'
      'FDConnection')
    TabOrder = 2
    OnClick = rgConnectTypeClick
  end
end
