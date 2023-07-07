unit Pas2JS.Compiler.Delphi;

interface

uses System.Classes, Pas2JSPCUCompiler, Pas2JSLogger, FPPJsSrcMap;

type
  TPas2JSCompiler = class(TPas2jsPCUCompiler)
  private
    function LoadFile(FileName: String; var Source: String): Boolean;
  protected
    function DoWriteJSFile(const DestFilename, MapFilename: String; Writer: TPas2JSMapper): Boolean; override;
  public
    procedure CheckUnitAlias(var UseUnitName: string); override;
    procedure Run(const FileName: String; const CommandLine: TStrings);
  end;

implementation

uses System.SysUtils, System.IOUtils;

{ TPas2JSCompiler }

procedure TPas2JSCompiler.CheckUnitAlias(var UseUnitName: string);

  function RemoveAlias(Alias: String): String;
  begin
    Alias := Alias + '.';

    if UseUnitName.StartsWith(Alias) then
      UseUnitName := UseUnitName.Substring(High(Alias));
  end;

begin
  RemoveAlias('System');
end;

function TPas2JSCompiler.DoWriteJSFile(const DestFilename, MapFilename: String; Writer: TPas2JSMapper): Boolean;
begin
  var DestinyFile := TStringStream.Create(EmptyStr, TEncoding.UTF8);
  Result := True;

  Writer.SaveJSToStream(True, MapFilename, DestinyFile);

  DestinyFile.SaveToFile(DestFilename);

  DestinyFile.Free;
end;

function TPas2JSCompiler.LoadFile(FileName: String; var Source: String): Boolean;
begin
  Result := True;
  Source := TFile.ReadAllText(FileName);
end;

procedure TPas2JSCompiler.Run(const FileName: String; const CommandLine: TStrings);
begin
  FileCache.OnReadFile := LoadFile;
  MainSrcFile := FileName;

  inherited Run(EmptyStr, EmptyStr, CommandLine, False);
end;

end.

