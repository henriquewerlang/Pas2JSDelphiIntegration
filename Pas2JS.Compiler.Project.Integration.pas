unit Pas2JS.Compiler.Project.Integration;

interface

uses System.Classes, System.Generics.Collections, Xml.XMLIntf, Pas2JS.Registry, ToolsAPI, Pas2JS.Compiler.Delphi, Pas2jsCompiler;

type
  TScriptType = (Style, Script, Module, Icon);

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
  SCRIPT_TYPE: array[TScriptType] of String = ('Style', 'Library', 'Module', 'Icon');

implementation

uses System.SysUtils, System.IOUtils, Rest.Types, Vcl.Dialogs, Winapi.Windows, Xml.XMLDoc, DCCStrs, Pas2JS.Consts, Pas2JS.Project.Options.Form;

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

  case ScriptType of
    Icon: LinkNode.Attributes['rel'] := 'icon';
    Module: LinkNode.Attributes['type'] := 'module';
    Style: LinkNode.Attributes['rel'] := 'stylesheet';
  end;
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
    var Configuration := TPas2JSProjectOptionForm.Create(nil);
    var Options := (FCurrentProject.ProjectOptions as IOTAProjectOptionsConfigurations).ActiveConfiguration;

    Compiler.Defines := Options.GetValue(sDefine, True);
    Compiler.OutputPath := GetOutputConfiguration;
    Compiler.SearchPath := Format('%s;%s', [ExpandMacros(Registry.LibraryPath), Options.Value[PAS2JS_SEARCH_PATH]]);

    Configuration.LoadConfiguration(Compiler, Options);

    Configuration.Free;
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
  Compiler.OnAddUnit :=
    procedure (FileName: TPas2jsCompilerFile)
    begin
      if not Compiler.AllJSIntoMainJS then
        AddScriptFile(ExtractFileName(ChangeFileExt(FileName.PasFileName, '.js')), Script);
    end;
  FCurrentProject := Project;

  try
    LoadCompilerConfiguration(Compiler);

    ForceDirectories(GetOutputConfiguration);

    StartIndexFile;

    Compiler.Run(ChangeFileExt(Project.FileName, '.dpr'));

    AddScriptFile(TPath.GetFileName(ChangeFileExt(Project.FileName, '.js')), Script);

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
var
  Configuration: IOTABuildConfiguration;
  HTML: IXMLNode;

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
    ModuleList.DelimitedText := Configuration.Value[PAS2JS_MODULES];

    for var A := 0 to Pred(ModuleList.Count) do
      AddScriptFile(ModuleList.KeyNames[A], GetScriptType(ModuleList.ValueFromIndex[A]));

    ModuleList.Free;
  end;

  procedure CopyResourceFiles;
  begin
    var ResourceDirectoryList := TStringList.Create;
    ResourceDirectoryList.DelimitedText := Configuration.Value[PAS2JS_RESOURCE_DIRECTORY_PATH];

    for var A := 0 to Pred(ResourceDirectoryList.Count) do
      TDirectory.Copy(ResourceDirectoryList.KeyNames[A], ExpandMacros(ResourceDirectoryList.ValueFromIndex[A].Replace('$(OutputDir)', GetOutputConfiguration, [rfIgnoreCase])));

    ResourceDirectoryList.Free;
  end;

  procedure LoadApplicationName;
  begin
    var ApplicationName := Configuration.Value[PAS2JS_APPLICATION_TITLE];

    if not ApplicationName.IsEmpty then
      FHeader.AddChild('Title').Text := ApplicationName
    else
      FHeader.AddChild('Title').Text := 'Untitle';
  end;

  procedure LoadApplicationIcon;
  begin
    var ApplicationIcon := Configuration.Value[PAS2JS_APPLICATION_ICON];

    if not ApplicationIcon.IsEmpty then
      AddScriptFile(ApplicationIcon, Icon);
  end;

  procedure LoadApplicationLanguage;
  begin
    var ApplicationLanguage := Configuration.Value[PAS2JS_APPLICATION_LANGUAGE];

    if not ApplicationLanguage.IsEmpty then
      HTML.Attributes['lang'] := ApplicationLanguage;
  end;

begin
  Configuration := (FCurrentProject.ProjectOptions as IOTAProjectOptionsConfigurations).ActiveConfiguration;
  FIndexFile := TXMLDocument.Create(nil);
  FIndexFile.XML.Text := '<!DOCTYPE html><html/>';

  FIndexFile.Active := True;
  FIndexFile.Options := [doNodeAutoIndent, doNodeAutoCreate];
  HTML := FIndexFile.DocumentElement;

  FHeader := HTML.ChildNodes['head'];

  LoadApplicationIcon;

  LoadApplicationName;

  LoadApplicationLanguage;

  AddScriptFiles;

  CopyResourceFiles;

  HTML.ChildNodes['body'].ChildNodes['script'].Text := 'rtl.run();';
end;

end.

