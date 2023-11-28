object DebugMain: TDebugMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'DebugMain'
  ClientHeight = 636
  ClientWidth = 575
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object lblFileToCompile: TLabel
    Left = 16
    Top = 8
    Width = 21
    Height = 15
    Caption = 'File:'
  end
  object lblSearchPath: TLabel
    Left = 16
    Top = 58
    Width = 65
    Height = 15
    Caption = 'Search path:'
  end
  object lblDefines: TLabel
    Left = 16
    Top = 108
    Width = 42
    Height = 15
    Caption = 'Defines:'
  end
  object lblOutputPath: TLabel
    Left = 16
    Top = 158
    Width = 68
    Height = 15
    Caption = 'Output path:'
  end
  object CompilerOutput: TSkLabel
    Left = 16
    Top = 423
    Width = 548
    Height = 202
    AutoSize = False
    TextSettings.VertAlign = Leading
    Words = <
      item
      end>
  end
  object CompilerExecute: TButton
    Left = 16
    Top = 392
    Width = 75
    Height = 25
    Caption = 'Execute'
    TabOrder = 13
    OnClick = CompilerExecuteClick
  end
  object FileToCompile: TEdit
    Left = 16
    Top = 29
    Width = 521
    Height = 23
    TabOrder = 0
    TextHint = 'Select a file to compile...'
  end
  object OpenFile: TButton
    Left = 543
    Top = 28
    Width = 21
    Height = 25
    Caption = '...'
    TabOrder = 1
    OnClick = OpenFileClick
  end
  object SearchPath: TEdit
    Left = 16
    Top = 79
    Width = 521
    Height = 23
    TabOrder = 2
    TextHint = 'The search path...'
  end
  object GenerateSingleFile: TCheckBox
    Left = 16
    Top = 208
    Width = 145
    Height = 17
    Caption = 'Generate single JS file'
    TabOrder = 5
  end
  object GenerateMapFile: TCheckBox
    Left = 16
    Top = 231
    Width = 145
    Height = 17
    Caption = 'Generate MAP File'
    TabOrder = 6
  end
  object EnumartorNumber: TCheckBox
    Left = 16
    Top = 277
    Width = 217
    Height = 17
    Caption = 'Generate enumerators as numbers'
    TabOrder = 8
  end
  object RemoveNotUsedPrivates: TCheckBox
    Left = 16
    Top = 300
    Width = 217
    Height = 17
    Caption = 'Remove not  used privates'
    TabOrder = 9
  end
  object RemoveNotUsedDeclaration: TCheckBox
    Left = 16
    Top = 323
    Width = 217
    Height = 17
    Caption = 'Remove not used declarations'
    TabOrder = 10
  end
  object DisableAllOptimizations: TCheckBox
    Left = 16
    Top = 254
    Width = 217
    Height = 17
    Caption = 'Disable all optimizations'
    TabOrder = 7
  end
  object RangeCheckError: TCheckBox
    Left = 16
    Top = 346
    Width = 217
    Height = 17
    Caption = 'Range check error'
    TabOrder = 11
  end
  object IntegerOverflowCheck: TCheckBox
    Left = 16
    Top = 369
    Width = 217
    Height = 17
    Caption = 'Integer overflow check'
    TabOrder = 12
  end
  object Defines: TEdit
    Left = 16
    Top = 129
    Width = 521
    Height = 23
    TabOrder = 3
    TextHint = 'Fill with the defines, comma separeted...'
  end
  object OutputPath: TEdit
    Left = 16
    Top = 179
    Width = 521
    Height = 23
    TabOrder = 4
    Text = '.\Output'
    TextHint = 'Output path...'
  end
  object SelectFile: TOpenDialog
    Left = 464
    Top = 220
  end
end
