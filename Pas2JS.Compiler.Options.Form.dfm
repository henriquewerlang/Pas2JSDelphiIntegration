object CompilerOptionsForm: TCompilerOptionsForm
  Left = 0
  Top = 0
  Caption = 'Options'
  ClientHeight = 434
  ClientWidth = 629
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object lblSearchPath: TLabel
    Left = 9
    Top = 6
    Width = 62
    Height = 15
    Caption = 'Search Path'
  end
  object lblSourceRoot: TLabel
    Left = 8
    Top = 332
    Width = 147
    Height = 15
    Caption = 'Prefix for source root in URL'
  end
  object lblRelativeSourceFolder: TLabel
    Left = 8
    Top = 382
    Width = 163
    Height = 15
    Caption = 'Relative directory source folder'
  end
  object SearchPath: TEdit
    Left = 9
    Top = 27
    Width = 612
    Height = 23
    TabOrder = 0
    TextHint = 'Search directories...'
  end
  object GenerateSingleFile: TCheckBox
    Left = 9
    Top = 56
    Width = 145
    Height = 17
    Caption = 'Generate single JS file'
    TabOrder = 1
  end
  object EnumaratoAsNumber: TCheckBox
    Left = 8
    Top = 79
    Width = 217
    Height = 17
    Caption = 'Generate enumerators as numbers'
    TabOrder = 2
  end
  object RemoveNotUsedPrivates: TCheckBox
    Left = 8
    Top = 102
    Width = 217
    Height = 17
    Caption = 'Remove not used privates'
    TabOrder = 3
  end
  object RemoveNotUsedDeclaration: TCheckBox
    Left = 8
    Top = 125
    Width = 217
    Height = 17
    Caption = 'Remove not used declarations'
    TabOrder = 4
  end
  object GenerateMapFile: TCheckBox
    Left = 8
    Top = 217
    Width = 145
    Height = 17
    Caption = 'Generate MAP File'
    TabOrder = 5
    OnClick = GenerateMapFileClick
  end
  object SourceRootFolder: TEdit
    Left = 8
    Top = 353
    Width = 269
    Height = 23
    TabOrder = 6
    TextHint = 'Source root for URL...'
  end
  object RelativeSourceFolder: TEdit
    Left = 8
    Top = 403
    Width = 269
    Height = 23
    TabOrder = 7
    TextHint = 'Relative source folder...'
  end
  object IncludeSourceInMapFile: TCheckBox
    Left = 8
    Top = 240
    Width = 269
    Height = 17
    Caption = 'Include the source file in the source map file'
    TabOrder = 8
  end
  object AbsoluteFileNames: TCheckBox
    Left = 8
    Top = 263
    Width = 205
    Height = 17
    Caption = 'Absolute file names for map file'
    TabOrder = 9
  end
  object XXSIProtection: TCheckBox
    Left = 8
    Top = 286
    Width = 205
    Height = 17
    Caption = 'Map file with XSSI protection'
    Checked = True
    State = cbChecked
    TabOrder = 10
  end
  object RangeCheckError: TCheckBox
    Left = 8
    Top = 148
    Width = 217
    Height = 17
    Caption = 'Range check error'
    TabOrder = 11
  end
  object IntegerOverflowCheck: TCheckBox
    Left = 8
    Top = 171
    Width = 217
    Height = 17
    Caption = 'Integer overflow check'
    TabOrder = 12
  end
  object CheckObjectsTypeCast: TCheckBox
    Left = 8
    Top = 194
    Width = 217
    Height = 17
    Caption = 'Check objects type cast'
    TabOrder = 13
  end
  object UseCORBAInterfaceImplementation: TCheckBox
    Left = 8
    Top = 309
    Width = 241
    Height = 17
    Caption = 'Use CORBA interface implementation'
    TabOrder = 14
  end
end
