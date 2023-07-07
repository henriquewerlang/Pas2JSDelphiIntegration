program Pas2JSCopyFiles;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.IOUtils, Vcl.Dialogs;

const
  EXTENSION_CONVERSION: array[Boolean] of String = ('.pas', '.pp');

  function GetFileNames: TArray<String>;
  begin
    Result := [
      '\FPCCompiler\packages\pastojs\src\fppjssrcmap.file',
      '\FPCCompiler\packages\pastojs\src\pas2jsfilecache.file',
      '\FPCCompiler\packages\pastojs\src\pas2jscompiler.file',
      '\FPCCompiler\packages\pastojs\src\pas2jslibcompiler.file',
      '\FPCCompiler\packages\pastojs\src\pas2jsutils.file',
      '\FPCCompiler\packages\pastojs\src\pas2jslogger.file',
      '\FPCCompiler\packages\pastojs\src\pas2jsfileutils.file',
      '\FPCCompiler\packages\pastojs\src\pas2jsfs.file',
      '\FPCCompiler\packages\pastojs\src\pas2jsresstrfile.file',
      '\FPCCompiler\packages\pastojs\src\pas2jshtmlresources.file',
      '\FPCCompiler\packages\pastojs\src\pas2jsresources.file',
      '\FPCCompiler\packages\pastojs\src\pas2jsjsresources.file',
      '\FPCCompiler\packages\pastojs\src\fppas2js.file',
      '\FPCCompiler\packages\pastojs\src\pas2jspparser.file',
      '\FPCCompiler\packages\pastojs\src\pas2jsuseanalyzer.file',
      '\FPCCompiler\packages\pastojs\src\pas2jspcucompiler.file',
      '\FPCCompiler\packages\pastojs\src\pas2jsfscompiler.file',
      '\FPCCompiler\packages\pastojs\src\pas2jsfiler.file',
      '\FPCCompiler\packages\pastojs\src\pas2jscompilercfg.file',
      '\FPCCompiler\packages\pastojs\src\pas2jscompilerpp.file',
      '\FPCCompiler\packages\fcl-js\src\jsbase.file',
      '\FPCCompiler\packages\fcl-js\src\jswriter.file',
      '\FPCCompiler\packages\fcl-js\src\jstree.file',
      '\FPCCompiler\packages\fcl-js\src\jstoken.file',
      '\FPCCompiler\packages\fcl-json\src\fpjson.file',
      '\FPCCompiler\packages\fcl-json\src\jsonparser.file',
      '\FPCCompiler\packages\fcl-json\src\jsonscanner.file',
      '\FPCCompiler\packages\fcl-json\src\jsonreader.file',
      '\FPCCompiler\packages\fcl-passrc\src\pscanner.file',
      '\FPCCompiler\packages\fcl-passrc\src\pasresolver.file',
      '\FPCCompiler\packages\fcl-passrc\src\pastree.file',
      '\FPCCompiler\packages\fcl-passrc\src\pparser.file',
      '\FPCCompiler\packages\fcl-process\src\process.file',
      '\FPCCompiler\packages\fcl-process\src\pipes.file',
      '\FPCCompiler\packages\fcl-base\src\avl_tree.file'];
  end;

begin
  var PasToFPC := ParamStr(2) = '1';
  var ProjectFolder := ParamStr(1);

  for var FileName in GetFileNames do
  begin
    var DestinyFile := ProjectFolder + ChangeFileExt(FileName, EXTENSION_CONVERSION[not PasToFPC]);
    var SourceFile := ProjectFolder + ChangeFileExt(FileName, EXTENSION_CONVERSION[PasToFPC]);

    if TFile.Exists(SourceFile) and not TFile.Exists(DestinyFile) then
      TFile.Move(SourceFile, DestinyFile);
  end;
end.

