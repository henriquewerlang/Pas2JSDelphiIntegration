unit Pas2JS.Project.Options.Form;

interface

uses Vcl.Forms, System.Classes, Vcl.Controls, Vcl.ControlList, Vcl.CustomizeDlg, Vcl.ExtCtrls, Soap.InvokeRegistry, Soap.WSDLIntf, Soap.SOAPPasInv, Soap.SOAPHTTPPasInv, Data.DB,
  Vcl.Grids, Vcl.DBGrids, Datasnap.DBClient, Vcl.StdCtrls, Vcl.ComCtrls, ToolsApi, Pas2JS.Compiler.Options.Form, Pas2JS.Compiler.Delphi;

type
  TPas2JSProjectOptionForm = class(TCompilerOptionsForm)
    btnOk: TButton;
    btnCancel: TButton;
    cobTarget: TComboBox;
    lblTarget: TLabel;
    cdsModules: TClientDataSet;
    cdsModulesSource: TStringField;
    cdsModulesModuleType: TStringField;
    dsModules: TDataSource;
    ResourceDirectory: TClientDataSet;
    ResourceDirectorySource: TStringField;
    ResourceDirectoryDestiny: TStringField;
    dsResourceDirectory: TDataSource;
    LabelResourceDirectory: TLabel;
    ResourceGrid: TDBGrid;
    lblModules: TLabel;
    GridModules: TDBGrid;
    ApplicationTitle: TEdit;
    lblApplicationTitle: TLabel;
    lblApplicationIcon: TLabel;
    ApplicationIcon: TEdit;
    procedure cobTargetSelect(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FSelectedConfiguration: IOTABuildConfiguration;

    function GetSelectedConfiguration: IOTABuildConfiguration;

    procedure ActivateConfiguration(const ItemIndex: Integer);
    procedure ClearConfiguration;
    procedure SaveConfiguration;
    procedure UpdateConfiguration(const Configuration: IOTABuildConfiguration);
  public
    procedure LoadConfiguration(const Compiler: TPas2JSCompilerDelphi; const Configuration: IOTABuildConfiguration);
    procedure Open;
  end;

  TConfiguration = class
  public
    Configuration: IOTABuildConfiguration;

    constructor Create(const Configuration: IOTABuildConfiguration);
  end;

implementation

{$R *.dfm}

uses System.SysUtils, Pas2JS.Consts, PlatformConst, Pas2JS.Compiler.Project.Integration;

{ TPas2JSProjectOptionForm }

procedure TPas2JSProjectOptionForm.ActivateConfiguration(const ItemIndex: Integer);
begin
  if Assigned(FSelectedConfiguration) then
    SaveConfiguration;

  cobTarget.ItemIndex := ItemIndex;
  FSelectedConfiguration := TConfiguration(cobTarget.Items.Objects[ItemIndex]).Configuration;

  UpdateConfiguration(GetSelectedConfiguration);
end;

procedure TPas2JSProjectOptionForm.btnOkClick(Sender: TObject);
begin
  SaveConfiguration;

  GetActiveProject.MarkModified;
end;

procedure TPas2JSProjectOptionForm.ClearConfiguration;
begin
  FSelectedConfiguration := nil;

  for var Configuration in cobTarget.Items.ToObjectArray do
    Configuration.Free;

  cobTarget.Items.Clear;
end;

procedure TPas2JSProjectOptionForm.cobTargetSelect(Sender: TObject);
begin
  ActivateConfiguration(cobTarget.ItemIndex);
end;

procedure TPas2JSProjectOptionForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ClearConfiguration;
end;

procedure TPas2JSProjectOptionForm.FormCreate(Sender: TObject);
begin
  GridModules.Columns[1].PickList.AddStrings(SCRIPT_TYPE);

  GridModules.Columns[1].DropDownRows := GridModules.Columns[1].PickList.Count;
end;

function TPas2JSProjectOptionForm.GetSelectedConfiguration: IOTABuildConfiguration;
begin
  Result := FSelectedConfiguration;
end;

procedure TPas2JSProjectOptionForm.LoadConfiguration(const Compiler: TPas2JSCompilerDelphi; const Configuration: IOTABuildConfiguration);
begin
  UpdateConfiguration(Configuration);

  inherited LoadConfiguration(Compiler);
end;

procedure TPas2JSProjectOptionForm.Open;
begin
  var Options := (GetActiveProject.ProjectOptions as IOTAProjectOptionsConfigurations);

  for var A := 0 to Pred(Options.ConfigurationCount) do
  begin
    var Configuration := Options.Configurations[A];

    cobTarget.Items.AddObject(Format('%s - All', [Configuration.Name]), TConfiguration.Create(Configuration));

    for var PlatformName in Configuration.Platforms do
    begin
      var PlatformConfiguration := Configuration.PlatformConfiguration[PlatformName];

      if PlatformConfiguration.Platform = cWin32Platform then
        cobTarget.Items.AddObject(Format('%s - %s', [Configuration.Name, PlatformConfiguration.Platform]), TConfiguration.Create(PlatformConfiguration));

      if (GetActiveProject.CurrentConfiguration = Configuration.Name) and (GetActiveProject.CurrentPlatform = PlatformConfiguration.Platform) then
        ActivateConfiguration(Pred(cobTarget.Items.Count));
    end;
  end;

  ShowModal;
end;

procedure TPas2JSProjectOptionForm.SaveConfiguration;
const
  BOOLEAN_VALUE: array[Boolean] of String = ('', 'true');

var
  Configuration: IOTABuildConfiguration;

  procedure SaveConfig(const PropertyName, Value: String); overload;
  begin
    if Configuration.PropertyExists(PropertyName) and Value.IsEmpty then
      Configuration.Remove(PropertyName)
    else
      Configuration.Value[PropertyName] := Value;
  end;

  procedure SaveConfig(const PropertyName: String; const Value: Boolean); overload;
  begin
    SaveConfig(PropertyName, BOOLEAN_VALUE[Value]);
  end;

  procedure SaveDataSet(const DataSet: TDataSet; const ConfigurationName: String);
  begin
    var Values := TStringList.Create(dupIgnore, False, False);
    DataSet.First;

    while not DataSet.Eof do
    begin
      Values.Values[DataSet.Fields[0].AsString] := DataSet.Fields[1].AsString;

      DataSet.Next;
    end;

    SaveConfig(ConfigurationName, Values.DelimitedText);

    Values.Free;
  end;

begin
  Configuration := GetSelectedConfiguration;

  SaveConfig(PAS2JS_ABSOLUTE_FILE_NAME_MAP_FILE, AbsoluteFileNames.Checked);
  SaveConfig(PAS2JS_APPLICATION_ICON, ApplicationIcon.Text);
  SaveConfig(PAS2JS_APPLICATION_TITLE, ApplicationTitle.Text);
  SaveConfig(PAS2JS_CHECK_OBJECT_TYPE_CAST, CheckObjectsTypeCast.Checked);
  SaveConfig(PAS2JS_ENUMERATOR_AS_NUMBER, EnumaratoAsNumber.Checked);
  SaveConfig(PAS2JS_GENERATE_MAP_FILE, GenerateMapFile.Checked);
  SaveConfig(PAS2JS_GENERATE_SINGLE_FILE, GenerateSingleFile.Checked);
  SaveConfig(PAS2JS_INCLUDE_SOURCE_MAP_FILE, IncludeSourceInMapFile.Checked);
  SaveConfig(PAS2JS_CHECK_INTEGER_OVERFLOW, IntegerOverflowCheck.Checked);
  SaveConfig(PAS2JS_MAP_FILE_PROTECTION, XXSIProtection.Checked);
  SaveConfig(PAS2JS_RANGE_CHECK_ERROR, RangeCheckError.Checked);
  SaveConfig(PAS2JS_RELATIVE_SOURCE_FOLDER, RelativeSourceFolder.Text);
  SaveConfig(PAS2JS_REMOVE_NOT_USED_DECLARATIONS, RemoveNotUsedDeclaration.Checked);
  SaveConfig(PAS2JS_REMOVE_NOT_USED_PRIVATES, RemoveNotUsedPrivates.Checked);
  SaveConfig(PAS2JS_SEARCH_PATH, SearchPath.Text);
  SaveConfig(PAS2JS_SOURCE_ROOT_FOLDER, SourceRootFolder.Text);
  SaveConfig(PAS2JS_USE_CORBA_INTERFACE, UseCORBAInterfaceImplementation.Checked);

  SaveDataSet(cdsModules, PAS2JS_MODULES);

  SaveDataSet(ResourceDirectory, PAS2JS_RESOURCE_DIRECTORY_PATH);
end;

procedure TPas2JSProjectOptionForm.UpdateConfiguration(const Configuration: IOTABuildConfiguration);

  procedure LoadDataSet(const DataSet: TClientDataSet; const ConfigurationName: String);
  begin
    var Values := TStringList.Create;
    Values.DelimitedText := Configuration.Value[ConfigurationName];

    DataSet.EmptyDataSet;

    for var A := 0  to Pred(Values.Count) do
    begin
      DataSet.Append;

      DataSet.Fields[0].AsString := Values.KeyNames[A];
      DataSet.Fields[1].AsString := Values.ValueFromIndex[A];

      DataSet.Post;
    end;

    Values.Free;
  end;

begin
  AbsoluteFileNames.Checked := Configuration.GetBoolean(PAS2JS_ABSOLUTE_FILE_NAME_MAP_FILE, False);
  ApplicationIcon.Text := Configuration.GetValue(PAS2JS_APPLICATION_ICON, False);
  ApplicationTitle.Text := Configuration.GetValue(PAS2JS_APPLICATION_TITLE, False);
  CheckObjectsTypeCast.Checked := Configuration.GetBoolean(PAS2JS_CHECK_OBJECT_TYPE_CAST, False);
  EnumaratoAsNumber.Checked := Configuration.GetBoolean(PAS2JS_ENUMERATOR_AS_NUMBER, False);
  GenerateMapFile.Checked := Configuration.GetBoolean(PAS2JS_GENERATE_MAP_FILE, False);
  GenerateSingleFile.Checked := Configuration.GetBoolean(PAS2JS_GENERATE_SINGLE_FILE, False);
  IncludeSourceInMapFile.Checked := Configuration.GetBoolean(PAS2JS_INCLUDE_SOURCE_MAP_FILE, False);
  IntegerOverflowCheck.Checked := Configuration.GetBoolean(PAS2JS_CHECK_INTEGER_OVERFLOW, False);
  RangeCheckError.Checked := Configuration.GetBoolean(PAS2JS_RANGE_CHECK_ERROR, False);
  RelativeSourceFolder.Text := Configuration.GetValue(PAS2JS_RELATIVE_SOURCE_FOLDER, False);
  RemoveNotUsedDeclaration.Checked := Configuration.GetBoolean(PAS2JS_REMOVE_NOT_USED_DECLARATIONS, False);
  RemoveNotUsedPrivates.Checked := Configuration.GetBoolean(PAS2JS_REMOVE_NOT_USED_PRIVATES, False);
  SearchPath.Text := Configuration.Value[PAS2JS_SEARCH_PATH];
  SourceRootFolder.Text := Configuration.GetValue(PAS2JS_SOURCE_ROOT_FOLDER, False);
  UseCORBAInterfaceImplementation.Checked := Configuration.GetBoolean(PAS2JS_USE_CORBA_INTERFACE, False);
  XXSIProtection.Checked := Configuration.GetBoolean(PAS2JS_MAP_FILE_PROTECTION, False);

  LoadDataSet(cdsModules, PAS2JS_MODULES);

  LoadDataSet(ResourceDirectory, PAS2JS_RESOURCE_DIRECTORY_PATH);

  CheckMapFileConfiguration;
end;

{ TConfiguration }

constructor TConfiguration.Create(const Configuration: IOTABuildConfiguration);
begin
  inherited Create;

  Self.Configuration := Configuration;
end;

end.

