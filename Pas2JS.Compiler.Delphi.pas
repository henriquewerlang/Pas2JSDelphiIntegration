unit Pas2JS.Compiler.Delphi;

interface

uses System.Classes, System.SysUtils, Pas2JSFScompiler, Pas2JSLogger, FPPJsSrcMap;

type
  TPas2JSCompilerDelphi = class(TPas2JSFSCompiler)
  private
    FOnReadFile: TProc<String>;

    function LoadFile(FileName: String; var Source: String): Boolean;
  protected
    function DoWriteJSFile(const DestFilename, MapFilename: String; Writer: TPas2JSMapper): Boolean; override;
  public
    procedure CheckUnitAlias(var UseUnitName: string); override;
    procedure Run(const FileName: String; const CommandLine: TStrings);

    property OnReadFile: TProc<String> read FOnReadFile write FOnReadFile;
  end;

implementation

uses System.IOUtils;

{ TPas2JSCompilerDelphi }

procedure TPas2JSCompilerDelphi.CheckUnitAlias(var UseUnitName: string);

  function RemoveAlias(Alias: String): String;
  begin
    Alias := Alias + '.';

    if UseUnitName.StartsWith(Alias) then
      UseUnitName := UseUnitName.Substring(High(Alias));
  end;

begin
  RemoveAlias('System');
end;

function TPas2JSCompilerDelphi.DoWriteJSFile(const DestFilename, MapFilename: String; Writer: TPas2JSMapper): Boolean;
begin
  var DestinyFile := TMemoryStream.Create;
  Result := True;

  Writer.SaveJSToStream(False, MapFilename, DestinyFile);

  DestinyFile.WriteData(#0);

  TFile.WriteAllText(DestFilename, PChar(DestinyFile.Memory), TEncoding.UTF8);

  DestinyFile.Free;
end;

function TPas2JSCompilerDelphi.LoadFile(FileName: String; var Source: String): Boolean;
begin
  Result := True;
  Source := TFile.ReadAllText(FileName);

  if Assigned(OnReadFile) then
    OnReadFile(Filename);
end;

procedure TPas2JSCompilerDelphi.Run(const FileName: String; const CommandLine: TStrings);
begin
  FileCache.OnReadFile := LoadFile;
  MainSrcFile := FileName;

  inherited Run(EmptyStr, EmptyStr, CommandLine, False);
end;

end.

