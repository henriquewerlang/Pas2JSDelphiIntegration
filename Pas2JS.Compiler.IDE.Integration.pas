unit Pas2JS.Compiler.IDE.Integration;

interface

uses Pas2JS.Compiler.Project.Integration, Vcl.Menus, ToolsApi, Pas2JS.Project.Options.Form;

type
  TPas2JSIDECompilerIntegration = class (TInterfacedObject, IOTAIDENotifier, IOTAIDENotifier50)
  private
    class var FNoticationIndex: Integer;
  private
    FCompiler: TPas2JSProjectCompiler;
    FOptionsForm: TPas2JSProjectOptionForm;
    FPas2JSMenu: TMenuItem;

    function GetCompiler: TPas2JSProjectCompiler;
    function GetOptionsForm: TPas2JSProjectOptionForm;
    function GetPas2JSMenu: TMenuItem;

    procedure AfterCompile(Succeeded: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
    procedure AfterSave;
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean); overload;
    procedure BeforeSave;
    procedure CheckMenu;
    procedure Destroyed;
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
    procedure Modified;
    procedure OnClickPas2JSMenu(Sender: TObject);

    property Compiler: TPas2JSProjectCompiler read GetCompiler;
    property OptionsForm: TPas2JSProjectOptionForm read GetOptionsForm;
    property Pas2JSMenu: TMenuItem read GetPas2JSMenu;

    class property NoticationIndex: Integer read FNoticationIndex write FNoticationIndex;
  public
    constructor Create;

    destructor Destroy; override;
  end;

procedure Register;

implementation

uses System.SysUtils, Vcl.Dialogs, Pas2JS.Consts;

procedure Register;
begin
  TPas2JSIDECompilerIntegration.NoticationIndex := (BorlandIDEServices as IOTAServices).AddNotifier(TPas2JSIDECompilerIntegration.Create);
end;

{ TPas2JSIDECompilerIntegration }

procedure TPas2JSIDECompilerIntegration.AfterCompile(Succeeded, IsCodeInsight: Boolean);
begin

end;

procedure TPas2JSIDECompilerIntegration.AfterSave;
begin

end;

procedure TPas2JSIDECompilerIntegration.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TPas2JSIDECompilerIntegration.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
end;

procedure TPas2JSIDECompilerIntegration.BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  Cancel := Project.ApplicationType = APPLICATION_TYPE;

  if Cancel then
    Compiler.Run(Project);
end;

procedure TPas2JSIDECompilerIntegration.BeforeSave;
begin

end;

procedure TPas2JSIDECompilerIntegration.CheckMenu;
begin
  Pas2JSMenu.Enabled := (GetActiveProject <> nil) and (GetActiveProject.ApplicationType = APPLICATION_TYPE);
end;

constructor TPas2JSIDECompilerIntegration.Create;
begin
  inherited;

  Pas2JSMenu.Caption := 'Pas2JS Options...';
  Pas2JSMenu.OnClick := OnClickPas2JSMenu;

  for var MenuItem in (BorlandIDEServices as INTAServices).MainMenu.Items do
    if MenuItem.Name = 'ProjectMenu' then
      MenuItem.Add(Pas2JSMenu);

  CheckMenu;
end;

destructor TPas2JSIDECompilerIntegration.Destroy;
begin
  FPas2JSMenu.Free;

  FOptionsForm.Free;

  inherited;
end;

procedure TPas2JSIDECompilerIntegration.Destroyed;
begin
  FreeAndNil(FCompiler);
end;

procedure TPas2JSIDECompilerIntegration.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
begin
  CheckMenu;
end;

function TPas2JSIDECompilerIntegration.GetCompiler: TPas2JSProjectCompiler;
begin
  if not Assigned(FCompiler) then
    FCompiler := TPas2JSProjectCompiler.Create;

  Result := FCompiler;
end;

function TPas2JSIDECompilerIntegration.GetOptionsForm: TPas2JSProjectOptionForm;
begin
  if not Assigned(FOptionsForm) then
    FOptionsForm := TPas2JSProjectOptionForm.Create(nil);

  Result := FOptionsForm;
end;

function TPas2JSIDECompilerIntegration.GetPas2JSMenu: TMenuItem;
begin
  if not Assigned(FPas2JSMenu) then
    FPas2JSMenu := TMenuItem.Create(nil);

  Result := FPas2JSMenu;
end;

procedure TPas2JSIDECompilerIntegration.Modified;
begin

end;

procedure TPas2JSIDECompilerIntegration.OnClickPas2JSMenu(Sender: TObject);
begin
  OptionsForm.Open;
end;

initialization

finalization
  (BorlandIDEServices as IOTAServices).RemoveNotifier(TPas2JSIDECompilerIntegration.NoticationIndex);

end.

