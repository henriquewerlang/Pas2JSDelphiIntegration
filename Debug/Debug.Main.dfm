inherited DebugMain: TDebugMain
  BorderIcons = [biSystemMenu]
  Caption = 'Debug Pas2JS'
  ClientHeight = 773
  ClientWidth = 603
  StyleElements = [seFont, seClient, seBorder]
  ExplicitWidth = 619
  ExplicitHeight = 812
  TextHeight = 15
  inherited lblSearchPath: TLabel
    Left = 8
    Top = 158
    StyleElements = [seFont, seClient, seBorder]
    ExplicitLeft = 8
    ExplicitTop = 158
  end
  inherited lblSourceRoot: TLabel
    Top = 534
    StyleElements = [seFont, seClient, seBorder]
    ExplicitTop = 534
  end
  inherited lblRelativeSourceFolder: TLabel
    Top = 584
    StyleElements = [seFont, seClient, seBorder]
    ExplicitTop = 584
  end
  object CompilerOutput: TSkLabel [3]
    Left = 8
    Top = 634
    Width = 587
    Height = 103
    Anchors = [akLeft, akTop, akRight, akBottom]
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
  object lblIncludePaths: TLabel [7]
    Left = 8
    Top = 208
    Width = 66
    Height = 15
    Caption = 'Include Path'
  end
  inherited SearchPath: TEdit
    Left = 8
    Top = 179
    Width = 560
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 11
    Text = '..\..\..\Pas2JS\packages\rtl\namespaced;'
    StyleElements = [seFont, seClient, seBorder]
    ExplicitLeft = 8
    ExplicitTop = 179
    ExplicitWidth = 560
  end
  inherited GenerateSingleFile: TCheckBox
    Left = 8
    Top = 258
    TabOrder = 12
    ExplicitLeft = 8
    ExplicitTop = 258
  end
  inherited EnumaratoAsNumber: TCheckBox
    Top = 281
    TabOrder = 13
    ExplicitTop = 281
  end
  inherited RemoveNotUsedPrivates: TCheckBox
    Top = 304
    ExplicitTop = 304
  end
  inherited RemoveNotUsedDeclaration: TCheckBox
    Top = 327
    ExplicitTop = 327
  end
  object CompilerExecute: TButton [13]
    Left = 8
    Top = 743
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Execute'
    Default = True
    TabOrder = 2
    OnClick = CompilerExecuteClick
  end
  object FileToCompile: TEdit [14]
    Left = 8
    Top = 29
    Width = 560
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    TextHint = 'Select a file to compile...'
  end
  object OpenFile: TButton [15]
    Left = 574
    Top = 28
    Width = 21
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = OpenFileClick
  end
  inherited GenerateMapFile: TCheckBox
    Top = 419
    ExplicitTop = 419
  end
  inherited SourceRootFolder: TEdit
    Top = 555
    StyleElements = [seFont, seClient, seBorder]
    ExplicitTop = 555
  end
  inherited RelativeSourceFolder: TEdit
    Top = 605
    StyleElements = [seFont, seClient, seBorder]
    ExplicitTop = 605
  end
  inherited IncludeSourceInMapFile: TCheckBox
    Top = 442
    ExplicitTop = 442
  end
  inherited AbsoluteFileNames: TCheckBox
    Top = 465
    ExplicitTop = 465
  end
  inherited XXSIProtection: TCheckBox
    Top = 488
    ExplicitTop = 488
  end
  object Defines: TEdit [22]
    Left = 8
    Top = 79
    Width = 560
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 19
    TextHint = 'Fill with the defines, comma separeted...'
  end
  object OutputPath: TEdit [23]
    Left = 8
    Top = 129
    Width = 560
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 15
    Text = '.\Output'
    TextHint = 'Output path...'
  end
  inherited RangeCheckError: TCheckBox
    Left = 9
    Top = 350
    TabOrder = 16
    ExplicitLeft = 9
    ExplicitTop = 350
  end
  inherited IntegerOverflowCheck: TCheckBox
    Left = 9
    Top = 373
    TabOrder = 17
    ExplicitLeft = 9
    ExplicitTop = 373
  end
  inherited CheckObjectsTypeCast: TCheckBox
    Top = 396
    TabOrder = 18
    ExplicitTop = 396
  end
  inherited UseCORBAInterfaceImplementation: TCheckBox
    Top = 511
    ExplicitTop = 511
  end
  object IncludePath: TEdit
    Left = 8
    Top = 229
    Width = 560
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 20
    TextHint = 'Include paths...'
  end
  object ShowCompilingMessages: TGroupBox
    Left = 283
    Top = 511
    Width = 185
    Height = 117
    Caption = ' Show compiling messages: '
    TabOrder = 21
    object ShowErrors: TCheckListBox
      Left = 2
      Top = 17
      Width = 181
      Height = 98
      Align = alClient
      ItemHeight = 17
      Items.Strings = (
        'Fatal'
        'Error'
        'Warning'
        'Hint'
        'Info')
      TabOrder = 0
      ExplicitHeight = 75
    end
  end
  object SelectFile: TOpenDialog
    Left = 487
    Top = 4
  end
end
