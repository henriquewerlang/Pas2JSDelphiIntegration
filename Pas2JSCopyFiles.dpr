program Pas2JSCopyFiles;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.IOUtils, Vcl.Dialogs;

const
  BASE_PATH = '\Pas2JS\Compiler\packages';
  EXTENSION_CONVERSION: array[Boolean] of String = ('.pas', '.pp');

  function GetFileNames: TArray<String>;
  begin
    Result := [
      '%s\pastojs\src\fppjssrcmap.file',
      '%s\pastojs\src\pas2jsfilecache.file',
      '%s\pastojs\src\pas2jscompiler.file',
      '%s\pastojs\src\pas2jslibcompiler.file',
      '%s\pastojs\src\pas2jsutils.file',
      '%s\pastojs\src\pas2jslogger.file',
      '%s\pastojs\src\pas2jsfileutils.file',
      '%s\pastojs\src\pas2jsfs.file',
      '%s\pastojs\src\pas2jsresstrfile.file',
      '%s\pastojs\src\pas2jshtmlresources.file',
      '%s\pastojs\src\pas2jsresources.file',
      '%s\pastojs\src\pas2jsjsresources.file',
      '%s\pastojs\src\fppas2js.file',
      '%s\pastojs\src\pas2jspparser.file',
      '%s\pastojs\src\pas2jsuseanalyzer.file',
      '%s\pastojs\src\pas2jspcucompiler.file',
      '%s\pastojs\src\pas2jsfscompiler.file',
      '%s\pastojs\src\pas2jsfiler.file',
      '%s\pastojs\src\pas2jscompilercfg.file',
      '%s\pastojs\src\pas2jscompilerpp.file',
      '%s\fcl-js\src\jsbase.file',
      '%s\fcl-js\src\jswriter.file',
      '%s\fcl-js\src\jstree.file',
      '%s\fcl-js\src\jstoken.file',
      '%s\fcl-json\src\fpjson.file',
      '%s\fcl-json\src\jsonparser.file',
      '%s\fcl-json\src\jsonscanner.file',
      '%s\fcl-json\src\jsonreader.file',
      '%s\fcl-passrc\src\pscanner.file',
      '%s\fcl-passrc\src\pasresolver.file',
      '%s\fcl-passrc\src\pastree.file',
      '%s\fcl-passrc\src\pparser.file',
      '%s\fcl-process\src\process.file',
      '%s\fcl-process\src\pipes.file',
      '%s\fcl-base\src\avl_tree.file'];
  end;

begin
  var PasToFPC := ParamStr(2) = '1';
  var ProjectFolder := ParamStr(1);

  for var FileNameList in GetFileNames do
  begin
    var FileName := Format(FileNameList, [BASE_PATH]);

    var DestinyFile := ProjectFolder + ChangeFileExt(FileName, EXTENSION_CONVERSION[not PasToFPC]);
    var SourceFile := ProjectFolder + ChangeFileExt(FileName, EXTENSION_CONVERSION[PasToFPC]);

    if TFile.Exists(SourceFile) and not TFile.Exists(DestinyFile) then
      TFile.Move(SourceFile, DestinyFile);
  end;
end.

