inherited DebugMain: TDebugMain
  BorderIcons = [biSystemMenu]
  Caption = 'Debug Pas2JS'
  ClientHeight = 751
  ClientWidth = 928
  ExplicitWidth = 944
  ExplicitHeight = 790
  TextHeight = 15
  inherited lblSearchPath: TLabel
    Left = 8
    Top = 158
    ExplicitLeft = 8
    ExplicitTop = 158
  end
  inherited lblSourceRoot: TLabel
    Top = 484
    ExplicitTop = 484
  end
  inherited lblRelativeSourceFolder: TLabel
    Top = 534
    ExplicitTop = 534
  end
  object CompilerOutput: TSkLabel [3]
    Left = 8
    Top = 588
    Width = 901
    Height = 127
    AutoSize = False
    TextSettings.VertAlign = Leading
    Words = <
      item
      end>
  end
  object lblFileToCompile: TLabel [4]
    Left = 8
    Top = 8
    Width = 21
    Height = 15
    Caption = 'File:'
  end
  object lblDefines: TLabel [5]
    Left = 8
    Top = 58
    Width = 42
    Height = 15
    Caption = 'Defines:'
  end
  object lblOutputPath: TLabel [6]
    Left = 8
    Top = 108
    Width = 68
    Height = 15
    Caption = 'Output path:'
  end
  inherited SearchPath: TEdit
    Left = 8
    Top = 179
    TabOrder = 11
    ExplicitLeft = 8
    ExplicitTop = 179
  end
  inherited GenerateSingleFile: TCheckBox
    Left = 8
    Top = 208
    TabOrder = 12
    ExplicitLeft = 8
    ExplicitTop = 208
  end
  inherited EnumaratoAsNumber: TCheckBox
    Top = 231
    TabOrder = 13
    ExplicitTop = 231
  end
  inherited RemoveNotUsedPrivates: TCheckBox
    Top = 254
    ExplicitTop = 254
  end
  inherited RemoveNotUsedDeclaration: TCheckBox
    Top = 277
    ExplicitTop = 277
  end
  object CompilerExecute: TButton [12]
    Left = 8
    Top = 721
    Width = 75
    Height = 25
    Caption = 'Execute'
    Default = True
    TabOrder = 2
    OnClick = CompilerExecuteClick
  end
  object FileToCompile: TEdit [13]
    Left = 8
    Top = 29
    Width = 521
    Height = 23
    TabOrder = 0
    TextHint = 'Select a file to compile...'
  end
  object OpenFile: TButton [14]
    Left = 535
    Top = 28
    Width = 21
    Height = 25
    Caption = '...'
    TabOrder = 1
    OnClick = OpenFileClick
  end
  inherited GenerateMapFile: TCheckBox
    Top = 369
    ExplicitTop = 369
  end
  inherited SourceRootFolder: TEdit
    Top = 505
    ExplicitTop = 505
  end
  inherited RelativeSourceFolder: TEdit
    Top = 555
    ExplicitTop = 555
  end
  inherited IncludeSourceInMapFile: TCheckBox
    Top = 392
    ExplicitTop = 392
  end
  inherited AbsoluteFileNames: TCheckBox
    Top = 415
    ExplicitTop = 415
  end
  inherited XXSIProtection: TCheckBox
    Top = 438
    ExplicitTop = 438
  end
  object Defines: TEdit [21]
    Left = 8
    Top = 79
    Width = 521
    Height = 23
    TabOrder = 19
    TextHint = 'Fill with the defines, comma separeted...'
  end
  object OutputPath: TEdit [22]
    Left = 8
    Top = 129
    Width = 521
    Height = 23
    TabOrder = 15
    Text = '.\Output'
    TextHint = 'Output path...'
  end
  inherited RangeCheckError: TCheckBox
    Left = 9
    Top = 300
    TabOrder = 16
    ExplicitLeft = 9
    ExplicitTop = 300
  end
  inherited IntegerOverflowCheck: TCheckBox
    Left = 9
    Top = 323
    TabOrder = 17
    ExplicitLeft = 9
    ExplicitTop = 323
  end
  inherited CheckObjectsTypeCast: TCheckBox
    Top = 346
    TabOrder = 18
    ExplicitTop = 346
  end
  inherited UseCORBAInterfaceImplementation: TCheckBox
    Top = 461
    ExplicitTop = 461
  end
  object SelectFile: TOpenDialog
    Left = 471
    Top = 324
  end
end
