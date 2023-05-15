object Pas2JSEnviromentOptions: TPas2JSEnviromentOptions
  Left = 0
  Top = 0
  Width = 399
  Height = 192
  TabOrder = 0
  DesignSize = (
    399
    192)
  object lblCompilerFile: TLabel
    Left = 0
    Top = 0
    Width = 125
    Height = 15
    Caption = 'Path to Pas2JS compiler'
  end
  object lblLibraryPath: TLabel
    Left = 0
    Top = 50
    Width = 63
    Height = 15
    Caption = 'Library path'
  end
  object Label1: TLabel
    Left = 3
    Top = 114
    Width = 63
    Height = 15
    Caption = 'Library path'
  end
  object FilePath: TEdit
    Left = 0
    Top = 21
    Width = 358
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    ExplicitWidth = 337
  end
  object btnSelectFile: TButton
    Left = 364
    Top = 21
    Width = 25
    Height = 23
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = btnSelectFileClick
    ExplicitLeft = 343
  end
  object LibraryPath: TEdit
    Left = 0
    Top = 71
    Width = 358
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    ExplicitWidth = 337
  end
  object SelectFile: TOpenDialog
    Filter = 'Pas2JS Library|pas2jslib.dll'
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 320
    Top = 24
  end
end
