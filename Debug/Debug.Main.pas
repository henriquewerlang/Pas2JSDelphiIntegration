unit Debug.Main;

interface

uses Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Classes, Vcl.Controls, System.Skia, Vcl.Skia, Pas2JS.Compiler.Options.Form, Data.DB, Datasnap.DBClient, Vcl.Grids, Vcl.DBGrids, Vcl.CheckLst;

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
    lblIncludePaths: TLabel;
    IncludePath: TEdit;
    ShowCompilingMessages: TGroupBox;
    ShowErrors: TCheckListBox;
    procedure OpenFileClick(Sender: TObject);
    procedure CompilerExecuteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  Compiler.InclusePath := IncludePath.Text;
  Compiler.OutputPath := OutputPath.Text;
  Compiler.OnCompilerMessage :=
    procedure(CompilerMessage: TCompilerMessage)

      function CompilerMessageChecked(var Color: TAlphaColor): Boolean;
      const
        COLOR_INDEX: array[0..4] of TAlphaColor = (TAlphaColors.Red, TAlphaColors.Crimson, TAlphaColors.Orange, TAlphaColors.Darkorchid, TAlphaColors.Black);

      begin
        var ItemIndex := ShowErrors.Items.IndexOf(CompilerMessage.&Type);

        Result := (ItemIndex > -1) and ShowErrors.Checked[ItemIndex];

        if Result then
          Color := COLOR_INDEX[ItemIndex];
      end;

    begin
      var Color: TAlphaColor;
      var Line := CompilerOutput.Words.Add;

      if CompilerMessageChecked(Color) then
      begin
        Line.Caption := Format('Line: %d, Column: %d - %s: %s'#13#10, [CompilerMessage.Line, CompilerMessage.Col, CompilerMessage.&Type, CompilerMessage.Message]);
        Line.FontColor := Color;
      end;
    end;
  Compiler.SearchPath := SearchPath.Text;
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

procedure TDebugMain.FormCreate(Sender: TObject);
begin
  inherited;

  ShowErrors.CheckAll(TCheckBoxState.cbChecked);
end;

procedure TDebugMain.OpenFileClick(Sender: TObject);
begin
  if SelectFile.Execute then
    FileToCompile.Text := SelectFile.FileName;
end;

end.

