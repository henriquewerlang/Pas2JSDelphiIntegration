inherited Pas2JSProjectOptionForm: TPas2JSProjectOptionForm
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Pas2JS Options'
  ClientHeight = 790
  ClientWidth = 1243
  Position = poDefaultSizeOnly
  StyleElements = [seFont, seClient, seBorder]
  OnClose = FormClose
  ExplicitWidth = 1259
  ExplicitHeight = 829
  TextHeight = 15
  inherited lblSearchPath: TLabel
    Left = 8
    Top = 58
    StyleElements = [seFont, seClient, seBorder]
    ExplicitLeft = 8
    ExplicitTop = 58
  end
  object lblTarget: TLabel [1]
    Left = 8
    Top = 8
    Width = 33
    Height = 15
    Caption = 'Target'
  end
  inherited lblSourceRoot: TLabel
    Left = 7
    Top = 361
    StyleElements = [seFont, seClient, seBorder]
    ExplicitLeft = 7
    ExplicitTop = 361
  end
  inherited lblRelativeSourceFolder: TLabel
    Left = 7
    Top = 411
    StyleElements = [seFont, seClient, seBorder]
    ExplicitLeft = 7
    ExplicitTop = 411
  end
  object LabelResourceDirectory: TLabel [4]
    Left = 8
    Top = 561
    Width = 99
    Height = 15
    Caption = 'Resource Directory'
    FocusControl = GridModules
  end
  object lblModules: TLabel [5]
    Left = 626
    Top = 561
    Width = 46
    Height = 15
    Caption = '&Modules'
    FocusControl = GridModules
  end
  object lblApplicationTitle: TLabel [6]
    Left = 8
    Top = 461
    Width = 77
    Height = 15
    Caption = 'Aplication title'
  end
  object lblApplicationIcon: TLabel [7]
    Left = 8
    Top = 511
    Width = 80
    Height = 15
    Caption = 'Aplication icon'
  end
  inherited SearchPath: TEdit
    Left = 8
    Top = 79
    TabOrder = 14
    StyleElements = [seFont, seClient, seBorder]
    ExplicitLeft = 8
    ExplicitTop = 79
  end
  inherited GenerateSingleFile: TCheckBox
    Left = 8
    Top = 108
    TabOrder = 17
    ExplicitLeft = 8
    ExplicitTop = 108
  end
  inherited EnumaratoAsNumber: TCheckBox
    Left = 7
    Top = 131
    ExplicitLeft = 7
    ExplicitTop = 131
  end
  inherited RemoveNotUsedPrivates: TCheckBox
    Left = 7
    Top = 154
    ExplicitLeft = 7
    ExplicitTop = 154
  end
  inherited RemoveNotUsedDeclaration: TCheckBox
    Left = 7
    Top = 177
    TabOrder = 18
    ExplicitLeft = 7
    ExplicitTop = 177
  end
  inherited GenerateMapFile: TCheckBox
    Left = 7
    Top = 269
    ExplicitLeft = 7
    ExplicitTop = 269
  end
  object btnOk: TButton [14]
    Left = 8
    Top = 760
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOkClick
  end
  object btnCancel: TButton [15]
    Left = 89
    Top = 760
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cobTarget: TComboBox [16]
    Left = 8
    Top = 29
    Width = 273
    Height = 23
    Style = csDropDownList
    TabOrder = 4
    TextHint = 'Target'
    OnSelect = cobTargetSelect
  end
  inherited SourceRootFolder: TEdit
    Left = 7
    Top = 382
    StyleElements = [seFont, seClient, seBorder]
    ExplicitLeft = 7
    ExplicitTop = 382
  end
  inherited RelativeSourceFolder: TEdit
    Left = 7
    Top = 432
    StyleElements = [seFont, seClient, seBorder]
    ExplicitLeft = 7
    ExplicitTop = 432
  end
  inherited IncludeSourceInMapFile: TCheckBox
    Left = 7
    Top = 292
    ExplicitLeft = 7
    ExplicitTop = 292
  end
  inherited AbsoluteFileNames: TCheckBox
    Left = 7
    Top = 315
    ExplicitLeft = 7
    ExplicitTop = 315
  end
  inherited XXSIProtection: TCheckBox
    Left = 7
    Top = 338
    ExplicitLeft = 7
    ExplicitTop = 338
  end
  inherited RangeCheckError: TCheckBox
    Left = 7
    Top = 200
    ExplicitLeft = 7
    ExplicitTop = 200
  end
  object ResourceGrid: TDBGrid [23]
    Left = 8
    Top = 582
    Width = 612
    Height = 172
    DataSource = dsResourceDirectory
    TabOrder = 15
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
  object GridModules: TDBGrid [24]
    Left = 626
    Top = 582
    Width = 612
    Height = 172
    DataSource = dsModules
    TabOrder = 16
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
  inherited IntegerOverflowCheck: TCheckBox
    Left = 7
    Top = 223
    ExplicitLeft = 7
    ExplicitTop = 223
  end
  inherited CheckObjectsTypeCast: TCheckBox
    Left = 7
    Top = 246
    ExplicitLeft = 7
    ExplicitTop = 246
  end
  object ApplicationTitle: TEdit
    Left = 8
    Top = 482
    Width = 268
    Height = 23
    TabOrder = 19
    TextHint = 'The aplication title...'
  end
  object ApplicationIcon: TEdit
    Left = 8
    Top = 532
    Width = 268
    Height = 23
    TabOrder = 20
    TextHint = 'The aplication icon file...'
  end
  object cdsModules: TClientDataSet
    PersistDataPacket.Data = {
      540000009619E0BD010000001800000002000000000003000000540006536F75
      726365020049000000010005574944544802000200E8030A4D6F64756C655479
      706501004900000001000557494454480200020032000000}
    Active = True
    Aggregates = <>
    Params = <>
    Left = 521
    Top = 44
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
    Left = 521
    Top = 100
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
    Left = 433
    Top = 44
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
    Left = 433
    Top = 100
  end
end
