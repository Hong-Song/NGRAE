object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'NG DB Utils'
  ClientHeight = 596
  ClientWidth = 861
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnLeft: TPanel
    Left = 0
    Top = 0
    Width = 257
    Height = 596
    Align = alLeft
    TabOrder = 0
    object SpeedButton1: TSpeedButton
      Left = 32
      Top = 176
      Width = 201
      Height = 57
      Caption = 'Create && Drop database'
    end
    object SpeedButton2: TSpeedButton
      Left = 32
      Top = 272
      Width = 201
      Height = 57
      Caption = 'Upgrade database'
    end
    object SpeedButton3: TSpeedButton
      Left = 32
      Top = 368
      Width = 201
      Height = 57
      Caption = 'Secure database'
    end
    object SpeedButton4: TSpeedButton
      Left = 32
      Top = 456
      Width = 201
      Height = 57
      Caption = 'Backup && Restore database'
    end
    object pnLogo: TPanel
      Left = 1
      Top = 1
      Width = 255
      Height = 144
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 263
    end
  end
  object pnRight: TPanel
    Left = 257
    Top = 0
    Width = 604
    Height = 596
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 440
    ExplicitTop = 112
    ExplicitWidth = 185
    ExplicitHeight = 41
    object pnDBConn: TPanel
      Left = 1
      Top = 1
      Width = 602
      Height = 144
      Align = alTop
      TabOrder = 0
    end
    object PageControl1: TPageControl
      Left = 1
      Top = 145
      Width = 602
      Height = 450
      ActivePage = TabSheet4
      Align = alClient
      TabOrder = 1
      object TabSheet1: TTabSheet
        Caption = 'TabSheet1'
      end
      object TabSheet2: TTabSheet
        Caption = 'TabSheet2'
        ImageIndex = 1
      end
      object TabSheet3: TTabSheet
        Caption = 'TabSheet3'
        ImageIndex = 2
      end
      object TabSheet4: TTabSheet
        Caption = 'TabSheet4'
        ImageIndex = 3
      end
    end
  end
end
