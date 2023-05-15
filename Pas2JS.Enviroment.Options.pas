unit Pas2JS.Enviroment.Options;

interface

uses ToolsApi, Vcl.Forms, Pas2JS.Enviroment.Options.Frame, Pas2JS.Registry;

type
  TPas2JSEnvironment = class(TInterfacedObject, INTAAddInOptions)
  private class var
    GNotifier: INTAAddInOptions;
  private
    FFrame: TPas2JSEnviromentOptions;
    FRegistry: TPas2JSRegistry;

    function GetArea: String;
    function GetCaption: String;
    function GetFrameClass: TCustomFrameClass;
    function GetHelpContext: Integer;
    function GetRegistry: TPas2JSRegistry;
    function IncludeInIDEInsight: Boolean;
    function ValidateContents: Boolean;

    class function GetNotifier: INTAAddInOptions; static;

    procedure DialogClosed(Accepted: Boolean);
    procedure FrameCreated(AFrame: TCustomFrame);

    property Registry: TPas2JSRegistry read GetRegistry;
  public
    destructor Destroy; override;

    class property Notifier: INTAAddInOptions read GetNotifier;
  end;

procedure Register;

implementation

uses System.SysUtils;

procedure Register;
begin
  (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(TPas2JSEnvironment.GetNotifier)
end;

{ TPas2JSEnvironment }

destructor TPas2JSEnvironment.Destroy;
begin
  FRegistry.Free;

  inherited;
end;

procedure TPas2JSEnvironment.DialogClosed(Accepted: Boolean);
begin
  if Accepted then
  begin
    Registry.CompilerPath := FFrame.FilePath.Text;
    Registry.LibraryPath := FFrame.LibraryPath.Text;
  end;
end;

procedure TPas2JSEnvironment.FrameCreated(AFrame: TCustomFrame);
begin
  FFrame := AFrame as TPas2JSEnviromentOptions;
  FFrame.FilePath.Text := Registry.CompilerPath;
  FFrame.LibraryPath.Text := Registry.LibraryPath;
end;

function TPas2JSEnvironment.GetArea: String;
begin
  Result := 'Pas2JS';
end;

function TPas2JSEnvironment.GetCaption: String;
begin
  Result := EmptyStr;
end;

function TPas2JSEnvironment.GetFrameClass: TCustomFrameClass;
begin
  Result := TPas2JSEnviromentOptions;
end;

function TPas2JSEnvironment.GetHelpContext: Integer;
begin
  Result := 0;
end;

class function TPas2JSEnvironment.GetNotifier: INTAAddInOptions;
begin
  if not Assigned(GNotifier) then
    GNotifier := TPas2JSEnvironment.Create;

  Result := GNotifier;
end;

function TPas2JSEnvironment.GetRegistry: TPas2JSRegistry;
begin
  if not Assigned(FRegistry) then
    FRegistry := TPas2JSRegistry.Create;

  Result := FRegistry;
end;

function TPas2JSEnvironment.IncludeInIDEInsight: Boolean;
begin
  Result := True;
end;

function TPas2JSEnvironment.ValidateContents: Boolean;
begin
  Result := True;
end;

initialization

finalization
  (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(TPas2JSEnvironment.GetNotifier);

end.

