unit Pas2JS.Compiler;

interface

uses Xml.XMLIntf, Pas2JS.Registry, ToolsAPI;

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
    FRegistry: TPas2JSRegistry;

    function BuildCommandLine(const Project: IOTAProject): TArray<UTF8String>;
    function GetCompiler: PPas2JSCompiler;
    function GetRegistry: TPas2JSRegistry;
    function GetOutputConfiguration(const Project: IOTAProject): String;
    function UnitAlias(const UnitName: PAnsiChar): Boolean;

    procedure AddScriptFile(const FileName: String);
    procedure CompilerLog(Info: String);
    procedure LoadLinks(const LibraryPath: String);
    procedure LoadPas2JSLibrary;
    procedure WriteJS(const FileName: PAnsiChar);
    procedure StartIndexFile(const Project: IOTAProject);

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

uses System.SysUtils, Rest.JSON, Vcl.Dialogs, Winapi.Windows, Xml.XMLDoc, DCCStrs;

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
  TPas2JSCompiler(Data).WriteJS(AFileName);
end;

{ TPas2JSCompiler }

procedure TPas2JSCompiler.AddScriptFile(const FileName: String);
begin
  FHeader.AddChild('script').Attributes['src'] := ExtractFileName(FileName);
end;

function TPas2JSCompiler.BuildCommandLine(const Project: IOTAProject): TArray<UTF8String>;
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
    var OutputPath := GetOutputConfiguration(Project);

    ForceDirectories(OutputPath);

    Result := Result + [UTF8String(Format('-FE%s', [OutputPath]))];
  end;

begin
  Options := (Project.ProjectOptions as IOTAProjectOptionsConfigurations).ActiveConfiguration;
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

function TPas2JSCompiler.GetOutputConfiguration(const Project: IOTAProject): String;
begin
  Result := ExpandMacros((Project.ProjectOptions as IOTAProjectOptionsConfigurations).ActiveConfiguration.GetValue(sExeOutput));
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
  var CommandLine := BuildCommandLine(Project);
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

  FIndexFile.SaveToFile(Format('%s\%s', [GetOutputConfiguration(Project), ExtractFileName(ChangeFileExt(Project.FileName, '.html'))]));
end;

procedure TPas2JSCompiler.StartIndexFile(const Project: IOTAProject);
begin
  FIndexFile := TXMLDocument.Create(nil);
  FIndexFile.Options := [doNodeAutoIndent];
  FIndexFile.Active := True;

//  FIndexFile.ChildNodes.Add(FIndexFile.CreateNode('html', ntDocType));

  var HTML := FIndexFile.AddChild('html');

  FHeader := HTML.AddChild('head');

  var Body := HTML.AddChild('body');
  Body.Attributes['type'] := 'application/javascript';
  Body.Text := 'rtl.run();';

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

    Result := Result + #0;
  end;

begin
  var NewName := UTF8String(RemoveAlias('System', String(UTF8String(UnitName))));
  Result := True;

  CopyMemory(UnitName, @NewName[1], Length(NewName));
end;

procedure TPas2JSCompiler.WriteJS(const FileName: PAnsiChar);
begin
  AddScriptFile(String(UTF8String(FileName)));
end;

end.

