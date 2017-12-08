object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 631
  ClientWidth = 1021
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
      Height = 152
      Align = alTop
      TabOrder = 0
      DesignSize = (
        583
        152)
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
      object btnMTBOpen: TButton
        Left = 328
        Top = 88
        Width = 250
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'btnMTBOpen'
        TabOrder = 3
        OnClick = btnMTBOpenClick
      end
      object btnOpen: TButton
        Left = 391
        Top = 119
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'btnOpen'
        TabOrder = 4
        OnClick = btnOpenClick
      end
      object btnApplyChanges: TButton
        Left = 472
        Top = 119
        Width = 106
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'btnApplyChanges'
        TabOrder = 5
        OnClick = btnApplyChangesClick
      end
    end
    object DBGrid1: TDBGrid
      Left = 1
      Top = 153
      Width = 583
      Height = 477
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
    Width = 436
    Height = 631
    Align = alClient
    TabOrder = 1
    object pnRightTop: TPanel
      Left = 1
      Top = 1
      Width = 434
      Height = 152
      Align = alTop
      TabOrder = 0
      DesignSize = (
        434
        152)
      object btnADOOpen1: TButton
        Left = 182
        Top = 0
        Width = 250
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'btnADOOpen1'
        TabOrder = 0
        OnClick = btnADOOpen1Click
      end
      object btnADOLoop: TButton
        Left = 183
        Top = 31
        Width = 250
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'btnADOLoop'
        TabOrder = 1
        OnClick = btnADOLoopClick
      end
      object btnADOLoop2: TButton
        Left = 183
        Top = 57
        Width = 250
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'btnADOLoop2'
        TabOrder = 2
        OnClick = btnADOLoopClick
      end
      object btnCdsOpen: TButton
        Left = 184
        Top = 81
        Width = 250
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'btnCdsOpen'
        TabOrder = 3
        OnClick = btnCdsOpenClick
      end
      object btnAODOpen: TButton
        Left = 231
        Top = 121
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'btnOpen'
        TabOrder = 4
        OnClick = btnAODOpenClick
      end
      object btnCdsApplayChanges: TButton
        Left = 320
        Top = 121
        Width = 106
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'btnApplyChanges'
        TabOrder = 5
        OnClick = btnCdsApplayChangesClick
      end
    end
    object DBGrid2: TDBGrid
      Left = 1
      Top = 153
      Width = 434
      Height = 477
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
    DataSet = mtb
    Left = 184
    Top = 248
  end
  object DataSource2: TDataSource
    DataSet = cds
    Left = 680
    Top = 200
  end
  object FDQuery1: TFDQuery
    FetchOptions.AssignedValues = [evRecordCountMode]
    FetchOptions.RecordCountMode = cmTotal
    Left = 264
    Top = 240
  end
  object cds: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 721
    Top = 288
  end
  object mtb: TNGFDMemTable
    CachedUpdates = True
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 360
    Top = 240
  end
end
