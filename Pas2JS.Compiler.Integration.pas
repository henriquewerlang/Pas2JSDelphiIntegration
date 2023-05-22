unit Pas2JS.Compiler.Integration;

interface

uses Pas2JS.Compiler, Vcl.Menus, ToolsApi, Pas2JS.Project.Options.Form;

type
  TPas2JSCompilerIntegration = class (TInterfacedObject, IOTAIDENotifier, IOTAIDENotifier50)
  private
    class var FNoticationIndex: Integer;
  private
    FCompiler: TPas2JSCompiler;
    FOptionsForm: TPas2JSProjectOptionForm;
    FPas2JSMenu: TMenuItem;

    function GetCompiler: TPas2JSCompiler;
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

    property Compiler: TPas2JSCompiler read GetCompiler;
    property OptionsForm: TPas2JSProjectOptionForm read GetOptionsForm;
    property Pas2JSMenu: TMenuItem read GetPas2JSMenu;

    class property NoticationIndex: Integer read FNoticationIndex write FNoticationIndex;
  public
    constructor Create;

    destructor Destroy; override;
  end;

procedure Register;

implementation

uses System.SysUtils, Pas2JS.Consts;

procedure Register;
begin
  TPas2JSCompilerIntegration.NoticationIndex := (BorlandIDEServices as IOTAServices).AddNotifier(TPas2JSCompilerIntegration.Create);
end;

{ TPas2JSCompilerIntegration }

procedure TPas2JSCompilerIntegration.AfterCompile(Succeeded, IsCodeInsight: Boolean);
begin

end;

procedure TPas2JSCompilerIntegration.AfterSave;
begin

end;

procedure TPas2JSCompilerIntegration.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TPas2JSCompilerIntegration.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
end;

procedure TPas2JSCompilerIntegration.BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  if Project.ApplicationType = APPLICATION_TYPE then
  begin
    Cancel := True;

    Compiler.Run(Project);
  end;
end;

procedure TPas2JSCompilerIntegration.BeforeSave;
begin

end;

procedure TPas2JSCompilerIntegration.CheckMenu;
begin
  Pas2JSMenu.Enabled := (GetActiveProject <> nil) and (GetActiveProject.ApplicationType = APPLICATION_TYPE);
end;

constructor TPas2JSCompilerIntegration.Create;
begin
  inherited;

  Pas2JSMenu.Caption := 'Pas2JS Options...';
  Pas2JSMenu.OnClick := OnClickPas2JSMenu;

  for var MenuItem in (BorlandIDEServices as INTAServices).MainMenu.Items do
    if MenuItem.Name = 'ProjectMenu' then
      MenuItem.Add(Pas2JSMenu);

  CheckMenu;
end;

destructor TPas2JSCompilerIntegration.Destroy;
begin
  FPas2JSMenu.Free;

  FOptionsForm.Free;

  inherited;
end;

procedure TPas2JSCompilerIntegration.Destroyed;
begin
  FreeAndNil(FCompiler);
end;

procedure TPas2JSCompilerIntegration.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
begin
  CheckMenu;
end;

function TPas2JSCompilerIntegration.GetCompiler: TPas2JSCompiler;
begin
  if not Assigned(FCompiler) then
    FCompiler := TPas2JSCompiler.Create;

  Result := FCompiler;
end;

function TPas2JSCompilerIntegration.GetOptionsForm: TPas2JSProjectOptionForm;
begin
  if not Assigned(FOptionsForm) then
    FOptionsForm := TPas2JSProjectOptionForm.Create(nil);

  Result := FOptionsForm;
end;

function TPas2JSCompilerIntegration.GetPas2JSMenu: TMenuItem;
begin
  if not Assigned(FPas2JSMenu) then
    FPas2JSMenu := TMenuItem.Create(nil);

  Result := FPas2JSMenu;
end;

procedure TPas2JSCompilerIntegration.Modified;
begin

end;

procedure TPas2JSCompilerIntegration.OnClickPas2JSMenu(Sender: TObject);
begin
  OptionsForm.Open;
end;

initialization

finalization
  (BorlandIDEServices as IOTAServices).RemoveNotifier(TPas2JSCompilerIntegration.NoticationIndex);

end.

