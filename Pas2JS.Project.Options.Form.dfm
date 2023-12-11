object Pas2JSProjectOptionForm: TPas2JSProjectOptionForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Pas2JS Options'
  ClientHeight = 769
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 15
  object lblModules: TLabel
    Left = 8
    Top = 428
    Width = 46
    Height = 15
    Caption = '&Modules'
    FocusControl = grdModule
  end
  object lblSearchPath: TLabel
    Left = 8
    Top = 58
    Width = 62
    Height = 15
    Caption = 'Search Path'
    FocusControl = grdModule
  end
  object lblTarget: TLabel
    Left = 8
    Top = 8
    Width = 33
    Height = 15
    Caption = 'Target'
    FocusControl = grdModule
  end
  object LabelResourceDirectory: TLabel
    Left = 8
    Top = 246
    Width = 99
    Height = 15
    Caption = 'Resource Directory'
    FocusControl = grdModule
  end
  object grdModule: TDBGrid
    Left = 8
    Top = 449
    Width = 612
    Height = 281
    DataSource = dsModules
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'Source'
        Width = 493
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ModuleType'
        Title.Caption = 'Type'
        Width = 79
        Visible = True
      end>
  end
  object btnOk: TButton
    Left = 8
    Top = 736
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 89
    Top = 736
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object edtSearchPath: TEdit
    Left = 8
    Top = 79
    Width = 612
    Height = 23
    TabOrder = 3
    TextHint = 'Search directories...'
  end
  object cbxGenerateSingleFile: TCheckBox
    Left = 8
    Top = 108
    Width = 145
    Height = 17
    Caption = 'Generate single JS file'
    TabOrder = 4
  end
  object cbxGenerateMapFile: TCheckBox
    Left = 8
    Top = 131
    Width = 145
    Height = 17
    Caption = 'Generate MAP File'
    TabOrder = 5
  end
  object cbxEnumartorNumber: TCheckBox
    Left = 8
    Top = 177
    Width = 217
    Height = 17
    Caption = 'Generate enumerators as numbers'
    TabOrder = 6
  end
  object cbxRemoveNotUsedPrivates: TCheckBox
    Left = 8
    Top = 200
    Width = 217
    Height = 17
    Caption = 'Remove not  used privates'
    TabOrder = 7
  end
  object cobTarget: TComboBox
    Left = 8
    Top = 29
    Width = 273
    Height = 23
    Style = csDropDownList
    TabOrder = 8
    TextHint = 'Target'
    OnSelect = cobTargetSelect
  end
  object cbxRemoveNotUsedDeclaration: TCheckBox
    Left = 8
    Top = 223
    Width = 217
    Height = 17
    Caption = 'Remove not used declarations'
    TabOrder = 9
  end
  object cbxDisableAllOptimizations: TCheckBox
    Left = 8
    Top = 154
    Width = 217
    Height = 17
    Caption = 'Disable all optimizations'
    TabOrder = 10
    OnClick = cbxDisableAllOptimizationsClick
  end
  object ResourceGrid: TDBGrid
    Left = 8
    Top = 268
    Width = 612
    Height = 154
    DataSource = dsResourceDirectory
    TabOrder = 11
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'Source'
        Width = 292
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Destiny'
        Width = 284
        Visible = True
      end>
  end
  object cdsModules: TClientDataSet
    PersistDataPacket.Data = {
      540000009619E0BD010000001800000002000000000003000000540006536F75
      726365020049000000010005574944544802000200E8030A4D6F64756C655479
      706501004900000001000557494454480200020032000000}
    Active = True
    Aggregates = <>
    Params = <>
    Left = 520
    Top = 96
    object cdsModulesSource: TStringField
      FieldName = 'Source'
      Size = 1000
    end
    object cdsModulesModuleType: TStringField
      FieldName = 'ModuleType'
      Size = 50
    end
  end
  object dsModules: TDataSource
    DataSet = cdsModules
    Left = 520
    Top = 152
  end
  object ResourceDirectory: TClientDataSet
    PersistDataPacket.Data = {
      510000009619E0BD010000001800000002000000000003000000510006536F75
      726365020049000000010005574944544802000200E8030744657374696E7902
      0049000000010005574944544802000200E8030000}
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'Source'
        DataType = ftString
        Size = 1000
      end
      item
        Name = 'Destiny'
        DataType = ftString
        Size = 1000
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 432
    Top = 96
    object ResourceDirectorySource: TStringField
      FieldName = 'Source'
      Size = 1000
    end
    object ResourceDirectoryDestiny: TStringField
      FieldName = 'Destiny'
      Size = 1000
    end
  end
  object dsResourceDirectory: TDataSource
    DataSet = ResourceDirectory
    Left = 432
    Top = 152
  end
end
