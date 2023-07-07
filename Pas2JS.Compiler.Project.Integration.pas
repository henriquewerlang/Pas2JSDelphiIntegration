unit Pas2JS.Compiler.Project.Integration;

interface

uses System.Classes, System.Generics.Collections, Xml.XMLIntf, Pas2JS.Registry, ToolsAPI, Pas2JS.Compiler.Delphi;

type
  TPas2JSProjectCompiler = class
  private
    FCommandLine: TStringList;
    FIndexFile: IXMLDocument;
    FHeader: IXMLNode;
    FCurrentProject: IOTAProject;
    FRegistry: TPas2JSRegistry;

    function BuildCommandLine: TStringList;
    function GetCommandLine: TStringList;
    function GetIndexFileName: String;
    function GetOutputConfiguration: String;
    function GetRegistry: TPas2JSRegistry;

    procedure AddScriptFile(const LinkFileName: String; const Script: Boolean);
    procedure CompilerLog(Sender: TObject; const Info: String);
    procedure LoadLinks(const LibraryPath: String);
    procedure LoadPas2JSLibrary;
    procedure SaveIndexFile;
    procedure StartIndexFile;
    procedure WriteJS(const FileName, FileContent: PAnsiChar; const ContentSize: Int32);

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

uses System.SysUtils, System.IOUtils, Rest.JSON, Rest.Types, Vcl.Dialogs, Winapi.Windows, Xml.XMLDoc, DCCStrs, Pas2JS.Consts;

function ExpandMacros(const Value: String): String;
begin
  Result := (BorlandIDEServices as IOTAServices).ExpandRootMacro(Value);
end;

procedure WriteJSCallBack(Data: Pointer; AFileName: PAnsiChar; AFileNameLen: Integer; AFileData: PAnsiChar; AFileDataLen: Int32); stdcall;
begin
  TPas2JSProjectCompiler(Data).WriteJS(AFileName, AFileData, AFileDataLen);
end;

{ TPas2JSProjectCompiler }

procedure TPas2JSProjectCompiler.AddScriptFile(const LinkFileName: String; const Script: Boolean);
begin
  var LinkAttributeName: String;
  var LinkNodeName: String;

  if Script then
  begin
    LinkAttributeName := 'src';
    LinkNodeName := 'script';
  end
  else
  begin
    LinkAttributeName := 'href';
    LinkNodeName := 'link';
  end;

  for var A := 0 to Pred(FHeader.ChildNodes.Count) do
  begin
    var Node := FHeader.ChildNodes[A];

    if Node.Attributes[LinkAttributeName] = LinkFileName then
      Exit;
  end;

  var LinkNode := FHeader.AddChild(LinkNodeName);
  LinkNode.Attributes[LinkAttributeName] := LinkFileName;

  if Script then
    LinkNode.Text := ''
  else
    LinkNode.Attributes['rel'] := 'stylesheet';
end;

function TPas2JSProjectCompiler.BuildCommandLine: TStringList;
var
  Options: IOTABuildConfiguration;

  procedure AppendCheckErrors;
  begin
    var CheckError := EmptyStr;

    if Options.GetBoolean(sRangeChecking) then
      CheckError := CheckError + 'r';

    if Options.GetBoolean(sIntegerOverflowCheck) then
      CheckError := CheckError + 'o';

    if CheckError <> '' then
      Result.Add('-C' + CheckError);
  end;

  procedure AppendSearchPaths;
  begin
    var IncludePath := Format('%s;%s', [ExpandMacros(Registry.LibraryPath), Options.Value[PAS2JS_SEARCH_PATH]]);
    Result.Add('-Fu' + IncludePath);
  end;

  procedure AppendDefines;
  begin
    var Defines := TStringList.Create;
    Defines.Delimiter := ';';
    Defines.DelimitedText := Options.GetValue(sDefine, True);

    for var DefineName in Defines do
      Result.Add('-d' + DefineName);
  end;

  procedure AppendOutputPath;
  begin
    var OutputPath := GetOutputConfiguration;

    ForceDirectories(OutputPath);

    Result.Add(Format('-FE%s', [OutputPath]));
  end;

  procedure AppendPas2JSConfigurations;
  begin
    if Options.AsBoolean[PAS2JS_GENERATE_SINGLE_FILE] then
      Result.Add('-Jc');

    if Options.AsBoolean[PAS2JS_GENERATE_MAP_FILE] then
      Result.Add('-Jm');

    if not Options.AsBoolean[PAS2JS_ENUMERATOR_AS_NUMBER] then
      Result.Add('-OoEnumNumbers-');

    if Options.AsBoolean[PAS2JS_REMOVE_PRIVATES] then
      Result.Add('-OoRemoveNotUsedPrivates');

    if Options.AsBoolean[PAS2JS_REMOVE_NOT_USED_DECLARATIONS] then
      Result.Add('-OoRemoveNotUsedDeclarations');
  end;

begin
  Options := (FCurrentProject.ProjectOptions as IOTAProjectOptionsConfigurations).ActiveConfiguration;
  Result := GetCommandLine;

  Result.AddStrings([
    '-JRjs',
    '-MDelphi',
    '-Pecmascript6',
    '-JeJSON',
    '-vewhqb'
  ]);

  AppendDefines;

  AppendCheckErrors;

  AppendSearchPaths;

  AppendOutputPath;

  AppendPas2JSConfigurations;

  Result.SaveToFile('C:\Teste\Config.txt');
end;

procedure TPas2JSProjectCompiler.CompilerLog(Sender: TObject; const Info: String);
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

constructor TPas2JSProjectCompiler.Create;
begin
  inherited Create;

  LoadPas2JSLibrary;
end;

destructor TPas2JSProjectCompiler.Destroy;
begin
  FRegistry.Free;

  inherited;
end;

function TPas2JSProjectCompiler.GetCommandLine: TStringList;
begin
  if not Assigned(FCommandLine) then
    FCommandLine := TStringList.Create;

  FCommandLine.Clear;

  Result := FCommandLine;
end;

function TPas2JSProjectCompiler.GetIndexFileName: String;
begin
  Result := Format('%s\index.html', [GetOutputConfiguration]);
end;

function TPas2JSProjectCompiler.GetOutputConfiguration: String;
begin
  Result := ExpandMacros((FCurrentProject.ProjectOptions as IOTAProjectOptionsConfigurations).ActiveConfiguration.GetValue(sExeOutput));
end;

function TPas2JSProjectCompiler.GetRegistry: TPas2JSRegistry;
begin
  if not Assigned(FRegistry) then
    FRegistry := TPas2JSRegistry.Create;

  Result := FRegistry;
end;

procedure TPas2JSProjectCompiler.LoadLinks(const LibraryPath: String);
begin

end;

procedure TPas2JSProjectCompiler.LoadPas2JSLibrary;
begin
  LoadLinks(Registry.CompilerPath);
end;

procedure TPas2JSProjectCompiler.Run(const Project: IOTAProject);
begin
  var Compiler := TPas2JSCompiler.Create;
  Compiler.Log.OnLog := CompilerLog;
  FCurrentProject := Project;

  var FileName := ChangeFileExt(Project.FileName, '.dpr');
  var FilePath := ExtractFilePath(FileName);

  StartIndexFile;

  Compiler.Run(FileName, BuildCommandLine);

  SaveIndexFile;

  Compiler.Free;
end;

procedure TPas2JSProjectCompiler.SaveIndexFile;
begin
  var Output := '<!DOCTYPE html>'#13#10 + FIndexFile.DocumentElement.XML;

  TFile.WriteAllText(GetIndexFileName, Output, TEncoding.UTF8);
end;

procedure TPas2JSProjectCompiler.StartIndexFile;

  procedure AddScriptFiles;
  begin
    var ModuleList := TStringList.Create;
    ModuleList.DelimitedText := (FCurrentProject.ProjectOptions as IOTAProjectOptionsConfigurations).ActiveConfiguration.Value[PAS2JS_MODULES];

    for var A := 0 to Pred(ModuleList.Count) do
      AddScriptFile(ModuleList.KeyNames[A], ModuleList.ValueFromIndex[A] = 'Library');

    AddScriptFile('rtl.js', True);

    ModuleList.Free;
  end;

begin
  FIndexFile := TXMLDocument.Create(nil);

  if TFile.Exists(GetIndexFileName) then
    FIndexFile.XML.Text := TFile.ReadAllText(GetIndexFileName)
  else
    FIndexFile.XML.Text := '<!DOCTYPE html><html/>';

  FIndexFile.Active := True;
  FIndexFile.Options := [doNodeAutoIndent, doNodeAutoCreate];
  var HTML := FIndexFile.DocumentElement;

  FHeader := HTML.ChildNodes['head'];

  HTML.ChildNodes['body'].ChildNodes['script'].Text := 'rtl.run();';

  AddScriptFiles;
end;

procedure TPas2JSProjectCompiler.WriteJS(const FileName, FileContent: PAnsiChar; const ContentSize: Int32);
begin
  var JSFileName := String(UTF8String(FileName));

  AddScriptFile(ExtractFileName(JSFileName), True);

  TFile.WriteAllBytes(JSFileName, BytesOf(FileContent, ContentSize));
end;

end.

