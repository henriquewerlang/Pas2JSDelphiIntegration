unit Pas2JS.Enviroment.Options.Frame;

interface

uses Vcl.Forms, System.Classes, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs;

type
  TPas2JSEnviromentOptions = class(TFrame)
    lblCompilerFile: TLabel;
    FilePath: TEdit;
    btnSelectFile: TButton;
    SelectFile: TOpenDialog;
    lblLibraryPath: TLabel;
    LibraryPath: TEdit;
    procedure btnSelectFileClick(Sender: TObject);
  end;

implementation

{$R *.dfm}

uses ToolsApi;

{ TPas2JSEnviromentOptions }

procedure TPas2JSEnviromentOptions.btnSelectFileClick(Sender: TObject);
begin
  if SelectFile.Execute then
    FilePath.Text := SelectFile.FileName;
end;

end.

