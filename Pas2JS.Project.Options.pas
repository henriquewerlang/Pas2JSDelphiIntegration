unit Pas2JS.Project.Options;

interface

uses ToolsApi, Xml.XMLIntf;

type
  TPas2JSProjectOptions = class(TInterfacedObject, IOTAProjectFileStorageNotifier)
  private class var
    GNotifier: IOTAProjectFileStorageNotifier;
  private
    class function GetNotifier: IOTAProjectFileStorageNotifier; static;

    function GetName: String;

    procedure CreatingProject(const ProjectOrGroup: IOTAModule);
    procedure ProjectClosing(const ProjectOrGroup: IOTAModule);
    procedure ProjectLoaded(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
    procedure ProjectSaving(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
  public
    class property Notifier: IOTAProjectFileStorageNotifier read GetNotifier;
  end;

procedure Register;

implementation

uses Vcl.Dialogs;

procedure Register;
begin
//  (BorlandIDEServices as IOTAProjectFileStorage).AddNotifier(TPas2JSProjectOptions.Notifier);
end;

{ TPas2JSProjectOptions }

procedure TPas2JSProjectOptions.CreatingProject(const ProjectOrGroup: IOTAModule);
begin

end;

function TPas2JSProjectOptions.GetName: String;
begin
  Result := 'Pas2JS';
end;

class function TPas2JSProjectOptions.GetNotifier: IOTAProjectFileStorageNotifier;
begin
  if not Assigned(GNotifier) then
    GNotifier := TPas2JSProjectOptions.Create;

  Result := GNotifier;
end;

procedure TPas2JSProjectOptions.ProjectClosing(const ProjectOrGroup: IOTAModule);
begin

end;

procedure TPas2JSProjectOptions.ProjectLoaded(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
begin

end;

procedure TPas2JSProjectOptions.ProjectSaving(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
begin

end;

end.

