unit Pas2JS.Compiler.Delphi;

{$SCOPEDENUMS ON}

interface

uses System.Classes, System.SysUtils, Pas2JSFScompiler, Pas2JSLogger, FPPJsSrcMap;

type
  TCompilerMessage = class;

  TCompilerOption = (RangeCheckError, IntegerOverflowCheck, GenerateSingleFile, GenerateMapFile, DisableAllOptimizations, GenerateEnumeratorNumber, RemoveNotUsedPrivates, RemoveNotUsedDeclaration);

  TCompilerOptions = set of TCompilerOption;

  TPas2JSCompilerDelphi = class(TPas2JSFSCompiler)
  private
    FOnCompilerMessage: TProc<TCompilerMessage>;
    FOnReadFile: TProc<String>;
    FOptions: TCompilerOptions;
    FSearchPath: String;
    FDefines: String;
    FOutputPath: String;

    function LoadFile(FileName: String; var Source: String): Boolean;

    procedure CompilerLog(Sender: TObject; const Info: String);
  protected
    function DoWriteJSFile(const DestFilename, MapFilename: String; Writer: TPas2JSMapper): Boolean; override;
  public
    procedure CheckUnitAlias(var UseUnitName: string); override;
    procedure Run(const FileName: String);

    property Defines: String read FDefines write FDefines;
    property OnCompilerMessage: TProc<TCompilerMessage> read FOnCompilerMessage write FOnCompilerMessage;
    property OnReadFile: TProc<String> read FOnReadFile write FOnReadFile;
    property Options: TCompilerOptions read FOptions write FOptions;
    property OutputPath: String read FOutputPath write FOutputPath;
    property SearchPath: String read FSearchPath write FSearchPath;
  end;

  TCompilerMessage = class
  private
    FCol: Integer;
    FFileName: String;
    FLine: Integer;
    FMessage: String;
    FNumber: Integer;
    FType: String;
  public
    property Col: Integer read FCol write FCol;
    property FileName: String read FFileName write FFileName;
    property Line: Integer read FLine write FLine;
    property Message: String read FMessage write FMessage;
    property Number: Integer read FNumber write FNumber;
    property &Type: String read FType write FType;
  end;

implementation

uses System.IOUtils, Rest.JSON, PasUseAnalyzer;

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

procedure TPas2JSCompilerDelphi.CompilerLog(Sender: TObject; const Info: String);
begin
  if Assigned(OnCompilerMessage) then
  begin
    var CompilerMessage := TJson.JsonToObject<TCompilerMessage>(Info);

    case CompilerMessage.Number of
      nPAUnitNotUsed: ;
      nPAParameterNotUsed: ;
      nPAParameterInOverrideNotUsed: ;
      else
      begin
        if CompilerMessage.Number > 0 then
          CompilerMessage.Message := Format('Code: %d %s', [CompilerMessage.Number, CompilerMessage.Message]);

        FOnCompilerMessage(CompilerMessage);
      end;
    end;

    CompilerMessage.Free;
  end;
end;

function TPas2JSCompilerDelphi.DoWriteJSFile(const DestFilename, MapFilename: String; Writer: TPas2JSMapper): Boolean;
begin
  var DestinyFile := TMemoryStream.Create;
  Result := True;

  Writer.SaveJSToStream(False, ExtractFileName(MapFilename), DestinyFile);

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

procedure TPas2JSCompilerDelphi.Run(const FileName: String);
var
  CommandLine: TStrings;

  procedure LoadCommandLine;
  begin
    var CheckError := EmptyStr;

    CommandLine.AddStrings([
      '-JRjs',
      '-MDelphi',
      '-Pecmascript6',
      '-JeJSON',
      '-vewhqb',
      '-Fu' + SearchPath,
      Format('-FE%s', [OutputPath])
    ]);

    if TCompilerOption.RangeCheckError in Options then
      CheckError := CheckError + 'r';

    if TCompilerOption.IntegerOverflowCheck in Options then
      CheckError := CheckError + 'o';

    if CheckError <> '' then
      CommandLine.Add('-C' + CheckError);

    for var DefineName in Defines.Split([';']) do
      CommandLine.Add('-d' + DefineName);

    if TCompilerOption.GenerateSingleFile in Options then
      CommandLine.Add('-Jc');

    if TCompilerOption.GenerateMapFile in Options then
      CommandLine.Add('-Jm');

    if TCompilerOption.DisableAllOptimizations in Options then
      CommandLine.Add('-O-')
    else
    begin
      if not (TCompilerOption.GenerateEnumeratorNumber in Options)  then
        CommandLine.Add('-OoEnumNumbers-');

      if not (TCompilerOption.RemoveNotUsedPrivates in Options) then
        CommandLine.Add('-OoRemoveNotUsedPrivates-');

      if TCompilerOption.RemoveNotUsedDeclaration in Options then
        CommandLine.Add('-OoRemoveNotUsedDeclarations');
    end;
  end;

begin
  CommandLine := TStringList.Create;
  FileCache.OnReadFile := LoadFile;
  Log.OnLog := CompilerLog;
  MainSrcFile := FileName;

  LoadCommandLine;

  try
    ForceDirectories(OutputPath);

    inherited Run(EmptyStr, EmptyStr, CommandLine, False);
  finally
    CommandLine.Free;
  end;
end;

end.

