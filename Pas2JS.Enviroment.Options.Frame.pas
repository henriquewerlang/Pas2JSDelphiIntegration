unit Pas2JS.Enviroment.Options.Frame;

interface

uses Vcl.Forms, System.Classes, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs;

type
  TPas2JSEnviromentOptions = class(TFrame)
    lblLibraryPath: TLabel;
    LibraryPath: TEdit;
  end;

implementation

{$R *.dfm}

uses ToolsApi;

{ TPas2JSEnviromentOptions }

end.

