unit Pas2JS.Compiler;

interface

uses Xml.XMLIntf, System.Generics.Collections, Pas2JS.Registry, ToolsAPI;

type
  PPas2JSCompiler = Pointer;
  TLibLogCallBack = procedure (Data: Pointer; Msg: PAnsiChar; MsgLen: Integer); stdcall;
  TUnitAliasCallBack = function (Data: Pointer; AUnitName: PAnsiChar; AUnitNameMaxLen: Integer): Boolean; stdcall;
  TWriteJSCallBack = procedure (Data : Pointer; AFileName: PAnsiChar; AFileNameLen : Integer; AFileData : PAnsiChar; AFileDataLen: Int32); stdcall;

  TPas2JSCompiler = class
  private
    FCompiler: PPas2JSCompiler;
    FGetPas2JSCompiler: function: PPas2JSCompiler; stdcall;
    FFreePas2JSCompiler: procedure (const P: PPas2JSCompiler); stdcall;
    FRunPas2JSCompiler: function(const P: PPas2JSCompiler; const ACompilerExe, AWorkingDir: PAnsiChar; const CommandLine: PPAnsiChar; const DoReset: Boolean): Boolean; stdcall;
    FSetPas2JSCompilerLogCallBack: procedure (P: PPas2JSCompiler; ACallBack: TLibLogCallBack; CallBackData: Pointer); stdcall;
    FSetPas2JSUnitAliasCallBack: procedure (P: PPas2JSCompiler; ACallBack: TUnitAliasCallBack; CallBackData: Pointer); stdcall;
    FSetPas2JSWriteJSCallBack: procedure (P: PPas2JSCompiler; ACallBack: TWriteJSCallBack; CallBackData: Pointer); stdcall;
  private
    FIndexFile: IXMLDocument;
    FHeader: IXMLNode;
    FCurrentProject: IOTAProject;
    FRegistry: TPas2JSRegistry;

    function BuildCommandLine: TArray<UTF8String>;
    function GetCompiler: PPas2JSCompiler;
    function GetIndexFileName: String;
    function GetOutputConfiguration: String;
    function GetRegistry: TPas2JSRegistry;
    function UnitAlias(const UnitName: PAnsiChar): Boolean;

    procedure AddScriptFile(const FileName: String);
    procedure CompilerLog(Info: String);
    procedure LoadLinks(const LibraryPath: String);
    procedure LoadPas2JSLibrary;
    procedure SaveIndexFile(const Project: IOTAProject);
    procedure StartIndexFile(const Project: IOTAProject);
    procedure WriteJS(const FileName, FileContent: PAnsiChar; const ContentSize: Int32);

    property Compiler: PPas2JSCompiler read GetCompiler;
    property Registry: TPas2JSRegistry read GetRegistry;
  public
    constructor Create;

    destructor Destroy; override;

    procedure Run(const Project: IOTAProject);
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

//  AddPas2JSDirectoryEntry = procedure (P: PDirectoryCache; AFilename: PAnsiChar; AAge: TPas2jsFileAgeTime; AAttr: TPas2jsFileAttr; ASize: TPas2jsFileSize); stdcall;
//  GetPas2JSCompilerLastError = procedure (P: PPas2JSCompiler; AError: PAnsiChar; var AErrorLength: Longint; AErrorClass: PAnsiChar; var AErrorClassLength: Longint); stdcall;
//  SetPas2JSReadDirCallBack = procedure (P: PPas2JSCompiler; ACallBack: TReadDirCallBack; CallBackData: Pointer); stdcall;
//  SetPas2JSReadPasCallBack = procedure (P: PPas2JSCompiler; ACallBack: TReadPasCallBack; CallBackData: Pointer; ABufferSize: Cardinal); stdcall;

uses System.SysUtils, System.IOUtils, Rest.JSON, Rest.Types, Vcl.Dialogs, Winapi.Windows, Xml.XMLDoc, DCCStrs;

procedure CompilerLogCallBack(Data: Pointer; Msg: PAnsiChar; MsgLen: Integer); stdcall;
begin
  TPas2JSCompiler(Data).CompilerLog(String(AnsiString(Msg)));
end;

function ExpandMacros(const Value: String): String;
begin
  Result := (BorlandIDEServices as IOTAServices).ExpandRootMacro(Value);
end;

function UnitAliasCallBack(Data: Pointer; AUnitName: PAnsiChar; AUnitNameMaxLen: Integer): Boolean; stdcall;
begin
  Result := TPas2JSCompiler(Data).UnitAlias(AUnitName);
end;

procedure WriteJSCallBack(Data: Pointer; AFileName: PAnsiChar; AFileNameLen: Integer; AFileData: PAnsiChar; AFileDataLen: Int32); stdcall;
begin
  TPas2JSCompiler(Data).WriteJS(AFileName, AFileData, AFileDataLen);
end;

{ TPas2JSCompiler }

procedure TPas2JSCompiler.AddScriptFile(const FileName: String);
begin
  var ScriptFile := ExtractFileName(FileName);

  for var A := 0 to Pred(FHeader.ChildNodes.Count) do
  begin
    var Node := FHeader.ChildNodes[A];

    if Node.Attributes['src'] = ScriptFile then
      Exit;
  end;

  var Script := FHeader.AddChild('script');
  Script.Attributes['src'] := ScriptFile;
  Script.Text := '';
end;

function TPas2JSCompiler.BuildCommandLine: TArray<UTF8String>;
var
  Options: IOTABuildConfiguration;

  procedure AppendCheckErrors;
  begin
    var CheckError: UTF8String := '';

    if Options.GetBoolean(sRangeChecking) then
      CheckError := CheckError + 'r';

    if Options.GetBoolean(sIntegerOverflowCheck) then
      CheckError := CheckError + 'o';

    if CheckError <> '' then
      Result := Result + ['-C' + CheckError];
  end;

  procedure AppendSearchPaths;
  begin
    var IncludePath := Format('%s;%s', [ExpandMacros(Options.GetValue(sUnitSearchPath, True)), ExpandMacros(Registry.LibraryPath)]);

    if IncludePath <> ';' then
      Result := Result + [UTF8String('-Fu' + IncludePath)];
  end;

  procedure AppendDefines;
  begin
    var Defines := Options.GetValue(sDefine, True);

    if not Defines.IsEmpty then
      Result := Result + [UTF8String('-d' + Defines)];
  end;

  procedure AppendOutputPath;
  begin
    var OutputPath := GetOutputConfiguration;

    ForceDirectories(OutputPath);

    Result := Result + [UTF8String(Format('-FE%s', [OutputPath]))];
  end;

begin
  Options := (FCurrentProject.ProjectOptions as IOTAProjectOptionsConfigurations).ActiveConfiguration;
  Result := [
    '-JRjs',
    '-MDelphi',
    '-Pecmascript6',
    '-JeJSON',
//    '-Jc', // Write all JavaScript concatenated into the output file
    '-vewhqb'
  ];

  AppendDefines;

  AppendCheckErrors;

  AppendSearchPaths;

  AppendOutputPath;
end;

procedure TPas2JSCompiler.CompilerLog(Info: String);
begin
  var CompilerMessage := TJson.JsonToObject<TCompilerMessage>(Info);
  var LineReference: Pointer;
  var MessageService := (BorlandIDEServices as IOTAMessageServices);
  var MessageType := otamkInfo;

  if CompilerMessage.&Type = 'Fatal' then
    MessageType := otamkFatal
  else if CompilerMessage.&Type = 'Error' then
    MessageType := otamkError
  else if CompilerMessage.&Type = 'Warning' then
    MessageType := otamkWarn
  else if CompilerMessage.&Type = 'Hint' then
    MessageType := otamkHint;

  MessageService.AddCompilerMessage(CompilerMessage.FileName, CompilerMessage.Message, 'Pas2JS Compiler', MessageType, CompilerMessage.Line, CompilerMessage.Col, nil, LineReference);

  MessageService := nil;

  CompilerMessage.Free;
end;

constructor TPas2JSCompiler.Create;
begin
  inherited Create;

  LoadPas2JSLibrary;
end;

destructor TPas2JSCompiler.Destroy;
begin
  if Assigned(FCompiler) then
    FFreePas2JSCompiler(FCompiler);

  FRegistry.Free;

  inherited;
end;

function TPas2JSCompiler.GetCompiler: PPas2JSCompiler;
begin
  if not Assigned(FCompiler) then
    FCompiler := FGetPas2JSCompiler;

  Result := FCompiler;
end;

function TPas2JSCompiler.GetIndexFileName: String;
begin
  Result := Format('%s\index.html', [GetOutputConfiguration]);
end;

function TPas2JSCompiler.GetOutputConfiguration: String;
begin
  Result := ExpandMacros((FCurrentProject.ProjectOptions as IOTAProjectOptionsConfigurations).ActiveConfiguration.GetValue(sExeOutput));
end;

function TPas2JSCompiler.GetRegistry: TPas2JSRegistry;
begin
  if not Assigned(FRegistry) then
    FRegistry := TPas2JSRegistry.Create;

  Result := FRegistry;
end;

procedure TPas2JSCompiler.LoadLinks(const LibraryPath: String);
begin
  var Handle := LoadLibrary(PChar(LibraryPath));

  if Handle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;

  FGetPas2JSCompiler := GetProcAddress(Handle, 'GetPas2JSCompiler');
  FFreePas2JSCompiler := GetProcAddress(Handle, 'FreePas2JSCompiler');
  FRunPas2JSCompiler := GetProcAddress(Handle, 'RunPas2JSCompiler');
  FSetPas2JSCompilerLogCallBack := GetProcAddress(Handle, 'SetPas2JSCompilerLogCallBack');
  FSetPas2JSUnitAliasCallBack := GetProcAddress(Handle, 'SetPas2JSUnitAliasCallBack');
  FSetPas2JSWriteJSCallBack := GetProcAddress(Handle, 'SetPas2JSWriteJSCallBack');
end;

procedure TPas2JSCompiler.LoadPas2JSLibrary;
begin
  LoadLinks(Registry.CompilerPath);
end;

procedure TPas2JSCompiler.Run(const Project: IOTAProject);
begin
  FCurrentProject := Project;

  var CommandLine := BuildCommandLine;
  var CommandLineParam: TArray<PAnsiChar> := nil;
  var FileName := UTF8String(ChangeFileExt(Project.FileName, '.dpr'));
  var FilePath := UTF8String(ExtractFilePath(String(FileName)));

  for var Param in CommandLine do
    CommandLineParam := CommandLineParam + [PAnsiChar(Param)];

  CommandLineParam := CommandLineParam + [PAnsiChar(FileName), nil];

  FSetPas2JSCompilerLogCallBack(Compiler, CompilerLogCallBack, Self);

  FSetPas2JSUnitAliasCallBack(Compiler, UnitAliasCallBack, Self);

  FSetPas2JSWriteJSCallBack(Compiler, WriteJSCallBack, Self);

  StartIndexFile(Project);

  FRunPas2JSCompiler(Compiler, nil, PAnsiChar(FilePath), @CommandLineParam[0], True);

  SaveIndexFile(Project);
end;

procedure TPas2JSCompiler.SaveIndexFile(const Project: IOTAProject);
begin
  var Output := '<!DOCTYPE html>'#13#10 + FIndexFile.DocumentElement.XML;

  TFile.WriteAllText(GetIndexFileName, Output, TEncoding.UTF8);
end;

procedure TPas2JSCompiler.StartIndexFile(const Project: IOTAProject);
begin
  FIndexFile := TXMLDocument.Create(nil);
  FIndexFile.Active := True;
  FIndexFile.Options := [doNodeAutoIndent, doNodeAutoCreate];
  var HTML: IXMLNode;

  if TFile.Exists(GetIndexFileName) then
  begin
    FIndexFile.LoadFromFile(GetIndexFileName);

    HTML := FIndexFile.DocumentElement;
  end
  else
    HTML := FIndexFile.AddChild('html');

  FHeader := HTML.ChildNodes['head'];

  HTML.ChildNodes['body'].ChildNodes['script'].Text := 'rtl.run();';

  AddScriptFile('rtl.js');
end;

function TPas2JSCompiler.UnitAlias(const UnitName: PAnsiChar): Boolean;

  function RemoveAlias(const Alias, UnitNameAlias: String): String;
  begin
    Result := Alias + '.';

    if UnitNameAlias.StartsWith(Result) then
      Result := UnitNameAlias.Substring(High(Result))
    else
      Result := UnitNameAlias;
  end;

begin
  var NewName := UTF8String(RemoveAlias('System', String(UTF8String(UnitName))));
  Result := True;

  AddScriptFile(String(NewName) + '.js');

  NewName := NewName + #0;

  CopyMemory(UnitName, @NewName[1], Length(NewName));
end;

procedure TPas2JSCompiler.WriteJS(const FileName, FileContent: PAnsiChar; const ContentSize: Int32);
begin
  var JSFileName := String(UTF8String(FileName));

  AddScriptFile(JSFileName);

  TFile.WriteAllBytes(JSFileName, BytesOf(FileContent, ContentSize));
end;

end.

