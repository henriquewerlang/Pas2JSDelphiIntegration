program Pas2JSCopyFiles;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.IOUtils, Vcl.Dialogs;

type
  EXTENSION_ARRAY = array[Boolean] of String;

var
  PasToFPC: Boolean;

  ProjectFolder: String;

  function GetFileNames: TArray<String>;
  begin
    Result := [
      'fcl-fpcunit\src\fpcunit.file',
      'fcl-fpcunit\src\testregistry.file',
      'fcl-fpcunit\src\testdecorator.file',
      'fcl-fpcunit\src\testutils.file',

      'fcl-base\src\avl_tree.file',
      'fcl-json\src\fpjson.file',
      'fcl-json\src\jsonparser.file',
      'fcl-json\src\jsonreader.file',
      'fcl-json\src\jsonscanner.file',
      'fcl-js\src\jsbase.file',
      'fcl-js\src\jstoken.file',
      'fcl-js\src\jstree.file',
      'fcl-js\src\jswriter.file',
      'fcl-passrc\src\pasresolver.file',
      'fcl-passrc\src\pastree.file',
      'fcl-passrc\src\pparser.file',
      'fcl-passrc\src\pscanner.file',
      'pastojs\src\fppas2js.file',
      'pastojs\src\fppjssrcmap.file',
      'pastojs\src\pas2jscompiler.file',
      'pastojs\src\pas2jsfilecache.file',
      'pastojs\src\pas2jsfileutils.file',
      'pastojs\src\pas2jsfs.file',
      'pastojs\src\pas2jsfscompiler.file',
      'pastojs\src\pas2jshtmlresources.file',
      'pastojs\src\pas2jsjsresources.file',
      'pastojs\src\pas2jslogger.file',
      'pastojs\src\pas2jspparser.file',
      'pastojs\src\pas2jsresources.file',
      'pastojs\src\pas2jsresstrfile.file',
      'pastojs\src\pas2jsuseanalyzer.file',
      'pastojs\src\pas2jsutils.file',
      '..\utils\pas2js\stubcreator.file'
      ];
  end;

  procedure ChangeFileExtension(const BaseFileName: String; const ExtensionConversor: EXTENSION_ARRAY);
  const
    BASE_PATH = '\Pas2JS\Compiler\packages\';

  begin
    var FileName := BASE_PATH + BaseFileName;

    var DestinyFile := ProjectFolder + ChangeFileExt(FileName, ExtensionConversor[not PasToFPC]);
    var SourceFile := ProjectFolder + ChangeFileExt(FileName, ExtensionConversor[PasToFPC]);

    if TFile.Exists(SourceFile) and not TFile.Exists(DestinyFile) then
      TFile.Move(SourceFile, DestinyFile);
  end;

const
  EXTENSION_FILE_CONVERSION: EXTENSION_ARRAY = ('.pas', '.pp');
  EXTENSION_PROJECT_CONVERSION: EXTENSION_ARRAY = ('.dpr', '.pp');

begin
  PasToFPC := ParamStr(2) = '1';
  ProjectFolder := ParamStr(1);

  ChangeFileExtension('pastojs\tests\testpas2js.file', EXTENSION_PROJECT_CONVERSION);

  for var FileName in GetFileNames do
    ChangeFileExtension(FileName, EXTENSION_FILE_CONVERSION);
end.

