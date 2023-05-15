unit Pas2JS.Compiler.Integration;

interface

uses Pas2JS.Compiler, ToolsApi;

type
  TPas2JSCompilerIntegration = class (TInterfacedObject, IOTAIDENotifier, IOTAIDENotifier50)
  private
    class var FNoticationIndex: Integer;
  private
    FCompiler: TPas2JSCompiler;

    function GetCompiler: TPas2JSCompiler;

    procedure AfterCompile(Succeeded: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
    procedure AfterSave;
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean); overload;
    procedure BeforeSave;
    procedure Destroyed;
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
    procedure Modified;

    property Compiler: TPas2JSCompiler read GetCompiler;

    class property NoticationIndex: Integer read FNoticationIndex write FNoticationIndex;
  end;

procedure Register;

implementation

uses System.SysUtils;

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
  if Project.ApplicationType = 'Web' then
  begin
    Cancel := True;

    Compiler.Run(Project);
  end;
end;

procedure TPas2JSCompilerIntegration.BeforeSave;
begin

end;

procedure TPas2JSCompilerIntegration.Destroyed;
begin
  FreeAndNil(FCompiler);
end;

procedure TPas2JSCompilerIntegration.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
begin

end;

function TPas2JSCompilerIntegration.GetCompiler: TPas2JSCompiler;
begin
  if not Assigned(FCompiler) then
    FCompiler := TPas2JSCompiler.Create;

  Result := FCompiler;
end;

procedure TPas2JSCompilerIntegration.Modified;
begin

end;

initialization

finalization
  (BorlandIDEServices as IOTAServices).RemoveNotifier(TPas2JSCompilerIntegration.NoticationIndex);

end.

