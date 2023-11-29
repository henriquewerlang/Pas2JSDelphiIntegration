unit Pas2JS.Compiler.Project.Integration;

interface

uses System.Classes, System.Generics.Collections, Xml.XMLIntf, Pas2JS.Registry, ToolsAPI, Pas2JS.Compiler.Delphi;

type
  TScriptType = (Style, Script, Module);

  TPas2JSProjectCompiler = class
  private
    FIndexFile: IXMLDocument;
    FHeader: IXMLNode;
    FCurrentProject: IOTAProject;
    FRegistry: TPas2JSRegistry;

    function GetIndexFileName: String;
    function GetOutputConfiguration: String;
    function GetRegistry: TPas2JSRegistry;

    procedure AddScriptFile(LinkFileName: String; const ScriptType: TScriptType);
    procedure SaveIndexFile;
    procedure StartIndexFile;

    property Registry: TPas2JSRegistry read GetRegistry;
  public
    destructor Destroy; override;

    procedure Run(const Project: IOTAProject);
  end;

const
  SCRIPT_TYPE: array[TScriptType] of String = ('Style', 'Library', 'Module');

implementation

uses System.SysUtils, System.IOUtils, Rest.Types, Vcl.Dialogs, Winapi.Windows, Xml.XMLDoc, DCCStrs, Pas2JS.Consts;

function ExpandMacros(const Value: String): String;
begin
  Result := (BorlandIDEServices as IOTAServices).ExpandRootMacro(Value);
end;

{ TPas2JSProjectCompiler }

procedure TPas2JSProjectCompiler.AddScriptFile(LinkFileName: String; const ScriptType: TScriptType);
begin
  var DestinyFile := Format('%s\%s', [GetOutputConfiguration, ExtractFileName(LinkFileName)]);
  var LinkAttributeName: String;
  var LinkNodeName: String;

  if TFile.Exists(LinkFileName) then
  begin
    TFile.Copy(LinkFileName, DestinyFile, True);

    LinkFileName := ExtractFileName(LinkFileName);
  end;

  if ScriptType in [Module, Script] then
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

  if ScriptType in [Module, Script] then
    LinkNode.NodeValue := EmptyStr;

  if ScriptType = Module then
    LinkNode.Attributes['type'] := 'module'
  else if ScriptType = Style then
    LinkNode.Attributes['rel'] := 'stylesheet';
end;

destructor TPas2JSProjectCompiler.Destroy;
begin
  FRegistry.Free;

  inherited;
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

procedure TPas2JSProjectCompiler.Run(const Project: IOTAProject);

  procedure LoadCompilerConfiguration(const Compiler: TPas2JSCompilerDelphi);
  begin
    var Options := (FCurrentProject.ProjectOptions as IOTAProjectOptionsConfigurations).ActiveConfiguration;

    Compiler.Defines := Options.GetValue(sDefine, True);
    Compiler.OutputPath := GetOutputConfiguration;
    Compiler.SearchPath := Format('%s;%s', [ExpandMacros(Registry.LibraryPath), Options.Value[PAS2JS_SEARCH_PATH]]);

    if Options.GetBoolean(sRangeChecking) then
      Compiler.Options := Compiler.Options + [TCompilerOption.RangeCheckError];

    if Options.GetBoolean(sIntegerOverflowCheck) then
      Compiler.Options := Compiler.Options + [TCompilerOption.IntegerOverflowCheck];

    if Options.AsBoolean[PAS2JS_GENERATE_SINGLE_FILE] then
      Compiler.Options := Compiler.Options + [TCompilerOption.GenerateSingleFile];

    if Options.AsBoolean[PAS2JS_GENERATE_MAP_FILE] then
      Compiler.Options := Compiler.Options + [TCompilerOption.GenerateMapFile];

    if Options.AsBoolean[PAS2JS_DISABLE_ALL_OPTIMIZATIONS] then
      Compiler.Options := Compiler.Options + [TCompilerOption.DisableAllOptimizations]
    else
    begin
      if not Options.AsBoolean[PAS2JS_ENUMERATOR_AS_NUMBER] then
        Compiler.Options := Compiler.Options + [TCompilerOption.GenerateEnumeratorNumber];

      if Options.AsBoolean[PAS2JS_REMOVE_NOT_USED_PRIVATES] then
        Compiler.Options := Compiler.Options + [TCompilerOption.RemoveNotUsedPrivates];

      if Options.AsBoolean[PAS2JS_REMOVE_NOT_USED_DECLARATIONS] then
        Compiler.Options := Compiler.Options + [TCompilerOption.RemoveNotUsedDeclaration];
    end;
  end;

begin
  var Compiler := TPas2JSCompilerDelphi.Create;
  Compiler.OnCompilerMessage :=
    procedure (CompilerMessage: TCompilerMessage)
    begin
      var LineReference: Pointer := nil;
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
    end;
  Compiler.OnReadFile :=
    procedure (FileName: String)
    begin
      if (TPath.GetExtension(FileName) = '.pas') or (TPath.GetExtension(FileName) = '.dpr') or (TPath.GetExtension(FileName) = '.pp') then
        AddScriptFile(TPath.GetFileName(TPath.ChangeExtension(FileName, '.js')), Script);
    end;
  FCurrentProject := Project;

  try
    LoadCompilerConfiguration(Compiler);

    ForceDirectories(GetOutputConfiguration);

    StartIndexFile;

    Compiler.Run(ChangeFileExt(Project.FileName, '.dpr'));

    SaveIndexFile;
  finally
    Compiler.Free;
  end;
end;

procedure TPas2JSProjectCompiler.SaveIndexFile;
begin
  var Output := '<!DOCTYPE html>'#13#10 + FIndexFile.DocumentElement.XML;

  TFile.WriteAllText(GetIndexFileName, Output, TEncoding.UTF8);
end;

procedure TPas2JSProjectCompiler.StartIndexFile;

  function GetScriptType(const TypeName: String): TScriptType;
  begin
    Result := Script;

    for var ScriptType := Low(TScriptType) to High(TScriptType) do
      if SCRIPT_TYPE[ScriptType] = TypeName then
        Exit(ScriptType);
  end;

  procedure AddScriptFiles;
  begin
    var ModuleList := TStringList.Create;
    ModuleList.DelimitedText := (FCurrentProject.ProjectOptions as IOTAProjectOptionsConfigurations).ActiveConfiguration.Value[PAS2JS_MODULES];

    for var A := 0 to Pred(ModuleList.Count) do
      AddScriptFile(ModuleList.KeyNames[A], GetScriptType(ModuleList.ValueFromIndex[A]));

    ModuleList.Free;
  end;

begin
  FIndexFile := TXMLDocument.Create(nil);
  FIndexFile.XML.Text := '<!DOCTYPE html><html/>';

  FIndexFile.Active := True;
  FIndexFile.Options := [doNodeAutoIndent, doNodeAutoCreate];
  var HTML := FIndexFile.DocumentElement;

  FHeader := HTML.ChildNodes['head'];

  HTML.ChildNodes['body'].ChildNodes['script'].Text := 'rtl.run();';

  AddScriptFiles;
end;

end.

