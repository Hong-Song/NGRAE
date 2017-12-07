object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 631
  ClientWidth = 859
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnLeft: TPanel
    Left = 0
    Top = 0
    Width = 585
    Height = 631
    Align = alLeft
    TabOrder = 0
    object pnLeftTop: TPanel
      Left = 1
      Top = 1
      Width = 583
      Height = 104
      Align = alTop
      TabOrder = 0
      DesignSize = (
        583
        104)
      object btnFDOpen1: TButton
        Left = 328
        Top = 0
        Width = 250
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'btnFDOpen1'
        TabOrder = 0
        OnClick = btnFDOpen1Click
      end
      object btnFDLoop: TButton
        Left = 328
        Top = 31
        Width = 250
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'btnFDLoop'
        TabOrder = 1
        OnClick = btnFDLoopClick
      end
      object btnFDLoop2: TButton
        Left = 328
        Top = 57
        Width = 250
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'btnFDLoop2'
        TabOrder = 2
        OnClick = btnFDLoopClick
      end
    end
    object DBGrid1: TDBGrid
      Left = 1
      Top = 105
      Width = 583
      Height = 525
      Align = alClient
      DataSource = DataSource1
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
  end
  object pnRight: TPanel
    Left = 585
    Top = 0
    Width = 274
    Height = 631
    Align = alClient
    TabOrder = 1
    object pnRightTop: TPanel
      Left = 1
      Top = 1
      Width = 272
      Height = 104
      Align = alTop
      TabOrder = 0
      DesignSize = (
        272
        104)
      object btnADOOpen1: TButton
        Left = 20
        Top = 0
        Width = 250
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'btnADOOpen1'
        TabOrder = 0
        OnClick = btnADOOpen1Click
      end
      object btnADOLoop: TButton
        Left = 21
        Top = 31
        Width = 250
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'btnADOLoop'
        TabOrder = 1
        OnClick = btnADOLoopClick
      end
      object btnADOLoop2: TButton
        Left = 21
        Top = 57
        Width = 250
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'btnADOLoop2'
        TabOrder = 2
        OnClick = btnADOLoopClick
      end
    end
    object DBGrid2: TDBGrid
      Left = 1
      Top = 105
      Width = 272
      Height = 525
      Align = alClient
      DataSource = DataSource2
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
  end
  object ADOQuery1: TADOQuery
    Parameters = <>
    Left = 760
    Top = 184
  end
  object DataSource1: TDataSource
    Left = 192
    Top = 328
  end
  object DataSource2: TDataSource
    Left = 664
    Top = 272
  end
  object FDQuery1: TFDQuery
    FetchOptions.AssignedValues = [evRecordCountMode]
    FetchOptions.RecordCountMode = cmTotal
    Left = 240
    Top = 248
  end
end
