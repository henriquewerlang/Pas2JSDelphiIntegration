unit Debug.Main;

interface

uses Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Classes, Vcl.Controls, System.Skia, Vcl.Skia, Pas2JS.Compiler.Options.Form, Data.DB, Datasnap.DBClient, Vcl.Grids, Vcl.DBGrids;

type
  TDebugMain = class(TCompilerOptionsForm)
    CompilerExecute: TButton;
    FileToCompile: TEdit;
    SelectFile: TOpenDialog;
    OpenFile: TButton;
    lblFileToCompile: TLabel;
    CompilerOutput: TSkLabel;
    lblDefines: TLabel;
    Defines: TEdit;
    lblOutputPath: TLabel;
    OutputPath: TEdit;
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
  Compiler.SearchPath := '..\..\..\Pas2JS\packages\rtl\namespaced;' + SearchPath.Text;
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

  LoadConfiguration(Compiler);

  CompilerOutput.Visible := False;

  try
    Compiler.Run(FileToCompile.Text);
  finally
    CompilerOutput.Visible := True;
  end;

  CompilerOutput.AutoSize := True;

  Compiler.Free;
end;

procedure TDebugMain.OpenFileClick(Sender: TObject);
begin
  if SelectFile.Execute then
    FileToCompile.Text := SelectFile.FileName;
end;

end.

