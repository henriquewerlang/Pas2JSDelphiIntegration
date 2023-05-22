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
    cbxRemovePrivates: TCheckBox;
    cobTarget: TComboBox;
    lblTarget: TLabel;
    cbxRemoveNotUsedDeclaration: TCheckBox;
    procedure cobTargetSelect(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FSelectedConfiguration: IOTABuildConfiguration;

    function GetSelectedConfiguration: IOTABuildConfiguration;

    procedure ActivateConfiguration(const ItemIndex: Integer);
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

uses System.SysUtils, Pas2JS.Consts, PlatformConst;

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
begin
  var Configuration := GetSelectedConfiguration;
  var Modules := TStringList.Create(dupIgnore, False, False);

  Configuration.AsBoolean[PAS2JS_ENUMERATOR_AS_NUMBER] := cbxEnumartorNumber.Checked;
  Configuration.AsBoolean[PAS2JS_GENERATE_MAP_FILE] := cbxGenerateMapFile.Checked;
  Configuration.AsBoolean[PAS2JS_GENERATE_SINGLE_FILE] := cbxGenerateSingleFile.Checked;
  Configuration.AsBoolean[PAS2JS_REMOVE_NOT_USED_DECLARATIONS] := cbxRemoveNotUsedDeclaration.Checked;
  Configuration.AsBoolean[PAS2JS_REMOVE_PRIVATES] := cbxRemovePrivates.Checked;

  if edtSearchPath.Text <> EmptyStr then
    Configuration.Value[PAS2JS_SEARCH_PATH] := edtSearchPath.Text;

  cdsModules.First;

  while not cdsModules.Eof do
  begin
    Modules.Values[cdsModulesSource.AsString] := cdsModulesModuleType.AsString;

    cdsModules.Next;
  end;

  if Configuration.Value[PAS2JS_MODULES] <> Modules.DelimitedText then
    Configuration.Value[PAS2JS_MODULES] := Modules.DelimitedText;

  Modules.Free;
end;

procedure TPas2JSProjectOptionForm.UpdateConfiguration;
begin
  var Configuration := GetSelectedConfiguration;
  var Modules := TStringList.Create;

  cbxEnumartorNumber.Checked := Configuration.GetBoolean(PAS2JS_ENUMERATOR_AS_NUMBER, False);
  cbxGenerateMapFile.Checked := Configuration.GetBoolean(PAS2JS_GENERATE_MAP_FILE, False);
  cbxGenerateSingleFile.Checked := Configuration.GetBoolean(PAS2JS_GENERATE_SINGLE_FILE, False);
  cbxRemoveNotUsedDeclaration.Checked := Configuration.GetBoolean(PAS2JS_REMOVE_NOT_USED_DECLARATIONS, False);
  cbxRemovePrivates.Checked := Configuration.GetBoolean(PAS2JS_REMOVE_PRIVATES, False);

  if Configuration.PropertyExists(PAS2JS_SEARCH_PATH) then
    edtSearchPath.Text := Configuration.GetValue(PAS2JS_SEARCH_PATH, False);

  if Configuration.PropertyExists(PAS2JS_MODULES) then
    Modules.DelimitedText := Configuration.GetValue(PAS2JS_MODULES, False);

  cdsModules.EmptyDataSet;

  for var A := 0  to Pred(Modules.Count) do
  begin
    cdsModules.Append;

    cdsModulesModuleType.AsString := Modules.ValueFromIndex[A];
    cdsModulesSource.AsString := Modules.KeyNames[A];

    cdsModules.Post;
  end;

  Modules.Free;
end;

{ TConfiguration }

constructor TConfiguration.Create(const Configuration: IOTABuildConfiguration);
begin
  inherited Create;

  Self.Configuration := Configuration;
end;

end.

