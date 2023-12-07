unit Debug.Main;

interface

uses Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Classes, Vcl.Controls, System.Skia, Vcl.Skia;

type
  TDebugMain = class(TForm)
    CompilerExecute: TButton;
    FileToCompile: TEdit;
    SelectFile: TOpenDialog;
    OpenFile: TButton;
    SearchPath: TEdit;
    GenerateSingleFile: TCheckBox;
    GenerateMapFile: TCheckBox;
    EnumartorNumber: TCheckBox;
    RemoveNotUsedPrivates: TCheckBox;
    RemoveNotUsedDeclaration: TCheckBox;
    DisableAllOptimizations: TCheckBox;
    lblFileToCompile: TLabel;
    lblSearchPath: TLabel;
    RangeCheckError: TCheckBox;
    IntegerOverflowCheck: TCheckBox;
    Defines: TEdit;
    lblDefines: TLabel;
    OutputPath: TEdit;
    lblOutputPath: TLabel;
    CompilerOutput: TSkLabel;
    procedure OpenFileClick(Sender: TObject);
    procedure CompilerExecuteClick(Sender: TObject);
  end;

var
  DebugMain: TDebugMain;

implementation

uses System.SysUtils, System.UITypes, Pas2JS.Compiler.Delphi;

{$R *.dfm}

{ TDebugMain }

procedure TDebugMain.CompilerExecuteClick(Sender: TObject);
begin
  var Compiler := TPas2JSCompilerDelphi.Create;
  Compiler.Defines := Defines.Text;
  Compiler.SearchPath := '..\..\..\Pas2JS\packages\rtl\src;' + SearchPath.Text;
  Compiler.OutputPath := OutputPath.Text;
  Compiler.OnCompilerMessage :=
    procedure(CompilerMessage: TCompilerMessage)
    begin
      var Line := CompilerOutput.Words.Add;

      if CompilerMessage.&Type = 'Fatal' then
        Line.FontColor := TAlphaColors.Red
      else if CompilerMessage.&Type = 'Error' then
        Line.FontColor := TAlphaColors.Crimson
      else if CompilerMessage.&Type = 'Warning' then
        Line.FontColor := TAlphaColors.Orange
      else if CompilerMessage.&Type = 'Hint' then
        Line.FontColor := TAlphaColors.Darkorchid;

      Line.Caption := Format('%s: %s'#13#10, [CompilerMessage.&Type, CompilerMessage.Message]);
    end;
  CompilerOutput.AutoSize := False;
  CompilerOutput.Caption := EmptyStr;

  if GenerateSingleFile.Checked then
    Compiler.Options := Compiler.Options + [TCompilerOption.GenerateSingleFile];

  if GenerateMapFile.Checked then
    Compiler.Options := Compiler.Options + [TCompilerOption.GenerateMapFile];

  if DisableAllOptimizations.Checked then
    Compiler.Options := Compiler.Options + [TCompilerOption.DisableAllOptimizations];

  if EnumartorNumber.Checked then
    Compiler.Options := Compiler.Options + [TCompilerOption.GenerateEnumeratorNumber];

  if RemoveNotUsedDeclaration.Checked then
    Compiler.Options := Compiler.Options + [TCompilerOption.RemoveNotUsedDeclaration];

  if RemoveNotUsedPrivates.Checked then
    Compiler.Options := Compiler.Options + [TCompilerOption.RemoveNotUsedPrivates];

  if RangeCheckError.Checked then
    Compiler.Options := Compiler.Options + [TCompilerOption.RangeCheckError];

  if IntegerOverflowCheck.Checked then
    Compiler.Options := Compiler.Options + [TCompilerOption.IntegerOverflowCheck];

  Compiler.Run(FileToCompile.Text);

  CompilerOutput.AutoSize := True;

  Compiler.Free;
end;

procedure TDebugMain.OpenFileClick(Sender: TObject);
begin
  if SelectFile.Execute then
    FileToCompile.Text := SelectFile.FileName;
end;

end.

