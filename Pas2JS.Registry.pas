unit Pas2JS.Registry;

interface

uses System.Win.Registry;

type
  TPas2JSRegistry = class
  private
    FRegistry: TRegistry;

    function GetCompilerPath: String;
    function GetLibraryPath: String;

    procedure SetCompilerPath(const Value: String);
    procedure SetLibraryPath(const Value: String);
  public
    constructor Create;

    destructor Destroy; override;

    property CompilerPath: String read GetCompilerPath write SetCompilerPath;
    property LibraryPath: String read GetLibraryPath write SetLibraryPath;
  end;

implementation

uses System.SysUtils, Winapi.Windows, ToolsApi;

const
  COMPILIER_PATH = 'Compiler Path';
  LIBRARY_PATH = 'Library Path';

{ TPas2JSRegistry }

constructor TPas2JSRegistry.Create;
begin
  inherited;

  FRegistry := TRegistry.Create;
  FRegistry.RootKey := HKEY_CURRENT_USER;

  FRegistry.OpenKey(Format('%s\Pas2JS', [(BorlandIDEServices as IOTAServices).GetBaseRegistryKey]), True);
end;

destructor TPas2JSRegistry.Destroy;
begin
  FRegistry.Free;

  inherited;
end;

function TPas2JSRegistry.GetCompilerPath: String;
begin
  Result := FRegistry.ReadString(COMPILIER_PATH);
end;

function TPas2JSRegistry.GetLibraryPath: String;
begin
  Result := FRegistry.ReadString(LIBRARY_PATH);
end;

procedure TPas2JSRegistry.SetCompilerPath(const Value: String);
begin
  FRegistry.WriteString(COMPILIER_PATH, Value);
end;

procedure TPas2JSRegistry.SetLibraryPath(const Value: String);
begin
  FRegistry.WriteString(LIBRARY_PATH, Value);
end;

end.

