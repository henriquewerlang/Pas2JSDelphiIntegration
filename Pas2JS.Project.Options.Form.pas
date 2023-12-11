unit Pas2JS.Project.Options.Form;

interface

uses Vcl.Forms, System.Classes, Vcl.Controls, Vcl.ControlList, Vcl.CustomizeDlg, Vcl.ExtCtrls, Soap.InvokeRegistry, Soap.WSDLIntf, Soap.SOAPPasInv, Soap.SOAPHTTPPasInv, Data.DB,
  Vcl.Grids, Vcl.DBGrids, Datasnap.DBClient, Vcl.StdCtrls, Vcl.ComCtrls, ToolsApi;

type
  TPas2JSProjectOptionForm = class(TForm)
    cdsModules: TClientDataSet;
    cdsModulesSource: TStringField;
    cdsModulesModuleType: TStringField;
    grdModule: TDBGrid;
    dsModules: TDataSource;
    btnOk: TButton;
    btnCancel: TButton;
    lblModules: TLabel;
    lblSearchPath: TLabel;
    edtSearchPath: TEdit;
    cbxGenerateSingleFile: TCheckBox;
    cbxGenerateMapFile: TCheckBox;
    cbxEnumartorNumber: TCheckBox;
    cbxRemoveNotUsedPrivates: TCheckBox;
    cobTarget: TComboBox;
    lblTarget: TLabel;
    cbxRemoveNotUsedDeclaration: TCheckBox;
    cbxDisableAllOptimizations: TCheckBox;
    LabelResourceDirectory: TLabel;
    ResourceGrid: TDBGrid;
    ResourceDirectory: TClientDataSet;
    dsResourceDirectory: TDataSource;
    ResourceDirectorySource: TStringField;
    ResourceDirectoryDestiny: TStringField;
    procedure cobTargetSelect(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbxDisableAllOptimizationsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FSelectedConfiguration: IOTABuildConfiguration;

    function GetSelectedConfiguration: IOTABuildConfiguration;

    procedure ActivateConfiguration(const ItemIndex: Integer);
    procedure CheckOptimizations;
    procedure ClearConfiguration;
    procedure SaveConfiguration;
    procedure UpdateConfiguration;
  public
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

  UpdateConfiguration;
end;

procedure TPas2JSProjectOptionForm.btnOkClick(Sender: TObject);
begin
  SaveConfiguration;

  GetActiveProject.MarkModified;
end;

procedure TPas2JSProjectOptionForm.cbxDisableAllOptimizationsClick(Sender: TObject);
begin
  CheckOptimizations;
end;

procedure TPas2JSProjectOptionForm.CheckOptimizations;
begin
  cbxEnumartorNumber.Enabled := not cbxDisableAllOptimizations.Checked;
  cbxRemoveNotUsedDeclaration.Enabled := not cbxDisableAllOptimizations.Checked;
  cbxRemoveNotUsedPrivates.Enabled := not cbxDisableAllOptimizations.Checked;
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
  grdModule.Columns[1].PickList.AddStrings(SCRIPT_TYPE);

  grdModule.Columns[1].DropDownRows := grdModule.Columns[1].PickList.Count;
end;

function TPas2JSProjectOptionForm.GetSelectedConfiguration: IOTABuildConfiguration;
begin
  Result := FSelectedConfiguration;
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

  procedure SaveConfig(const PropertyName, Value: String);
  begin
    if Configuration.PropertyExists(PropertyName) and Value.IsEmpty then
      Configuration.Remove(PropertyName)
    else
      Configuration.Value[PropertyName] := Value;
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

  SaveConfig(PAS2JS_DISABLE_ALL_OPTIMIZATIONS, BOOLEAN_VALUE[cbxDisableAllOptimizations.Checked]);
  SaveConfig(PAS2JS_ENUMERATOR_AS_NUMBER, BOOLEAN_VALUE[cbxEnumartorNumber.Checked]);
  SaveConfig(PAS2JS_GENERATE_MAP_FILE, BOOLEAN_VALUE[cbxGenerateMapFile.Checked]);
  SaveConfig(PAS2JS_GENERATE_SINGLE_FILE, BOOLEAN_VALUE[cbxGenerateSingleFile.Checked]);
  SaveConfig(PAS2JS_REMOVE_NOT_USED_DECLARATIONS, BOOLEAN_VALUE[cbxRemoveNotUsedDeclaration.Checked]);
  SaveConfig(PAS2JS_REMOVE_NOT_USED_PRIVATES, BOOLEAN_VALUE[cbxRemoveNotUsedPrivates.Checked]);
  SaveConfig(PAS2JS_SEARCH_PATH, edtSearchPath.Text);

  SaveDataSet(cdsModules, PAS2JS_MODULES);

  SaveDataSet(ResourceDirectory, PAS2JS_RESOURCE_DIRECTORY_PATH);
end;

procedure TPas2JSProjectOptionForm.UpdateConfiguration;
var
  Configuration: IOTABuildConfiguration;

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
  Configuration := GetSelectedConfiguration;

  cbxDisableAllOptimizations.Checked := Configuration.GetBoolean(PAS2JS_DISABLE_ALL_OPTIMIZATIONS, False);
  cbxEnumartorNumber.Checked := Configuration.GetBoolean(PAS2JS_ENUMERATOR_AS_NUMBER, False);
  cbxGenerateMapFile.Checked := Configuration.GetBoolean(PAS2JS_GENERATE_MAP_FILE, False);
  cbxGenerateSingleFile.Checked := Configuration.GetBoolean(PAS2JS_GENERATE_SINGLE_FILE, False);
  cbxRemoveNotUsedDeclaration.Checked := Configuration.GetBoolean(PAS2JS_REMOVE_NOT_USED_DECLARATIONS, False);
  cbxRemoveNotUsedPrivates.Checked := Configuration.GetBoolean(PAS2JS_REMOVE_NOT_USED_PRIVATES, False);
  edtSearchPath.Text := Configuration.Value[PAS2JS_SEARCH_PATH];

  LoadDataSet(cdsModules, PAS2JS_MODULES);

  LoadDataSet(ResourceDirectory, PAS2JS_RESOURCE_DIRECTORY_PATH);

  CheckOptimizations;
end;

{ TConfiguration }

constructor TConfiguration.Create(const Configuration: IOTABuildConfiguration);
begin
  inherited Create;

  Self.Configuration := Configuration;
end;

end.

