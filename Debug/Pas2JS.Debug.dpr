program Pas2JS.Debug;

uses
  Vcl.Forms,
  Delphi.Helper in '..\Helper\Delphi.Helper.pas',
  fppjssrcmap in '..\Compiler\packages\pastojs\src\fppjssrcmap.pas',
  pas2jsfilecache in '..\Compiler\packages\pastojs\src\pas2jsfilecache.pas',
  pas2jscompiler in '..\Compiler\packages\pastojs\src\pas2jscompiler.pas',
  pas2jsutils in '..\Compiler\packages\pastojs\src\pas2jsutils.pas',
  pas2jslogger in '..\Compiler\packages\pastojs\src\pas2jslogger.pas',
  pas2jsfileutils in '..\Compiler\packages\pastojs\src\pas2jsfileutils.pas',
  pas2jsfs in '..\Compiler\packages\pastojs\src\pas2jsfs.pas',
  pas2jsresstrfile in '..\Compiler\packages\pastojs\src\pas2jsresstrfile.pas',
  pas2jshtmlresources in '..\Compiler\packages\pastojs\src\pas2jshtmlresources.pas',
  pas2jsresources in '..\Compiler\packages\pastojs\src\pas2jsresources.pas',
  pas2jsjsresources in '..\Compiler\packages\pastojs\src\pas2jsjsresources.pas',
  fppas2js in '..\Compiler\packages\pastojs\src\fppas2js.pas',
  pas2jspparser in '..\Compiler\packages\pastojs\src\pas2jspparser.pas',
  pas2jsuseanalyzer in '..\Compiler\packages\pastojs\src\pas2jsuseanalyzer.pas',
  pas2jsfscompiler in '..\Compiler\packages\pastojs\src\pas2jsfscompiler.pas',
  jsbase in '..\Compiler\packages\fcl-js\src\jsbase.pas',
  jswriter in '..\Compiler\packages\fcl-js\src\jswriter.pas',
  jstree in '..\Compiler\packages\fcl-js\src\jstree.pas',
  jstoken in '..\Compiler\packages\fcl-js\src\jstoken.pas',
  jssrcmap in '..\Compiler\packages\fcl-js\src\jssrcmap.pas',
  fpjson in '..\Compiler\packages\fcl-json\src\fpjson.pas',
  jsonparser in '..\Compiler\packages\fcl-json\src\jsonparser.pas',
  jsonscanner in '..\Compiler\packages\fcl-json\src\jsonscanner.pas',
  jsonreader in '..\Compiler\packages\fcl-json\src\jsonreader.pas',
  pscanner in '..\Compiler\packages\fcl-passrc\src\pscanner.pas',
  pasresolver in '..\Compiler\packages\fcl-passrc\src\pasresolver.pas',
  pastree in '..\Compiler\packages\fcl-passrc\src\pastree.pas',
  pparser in '..\Compiler\packages\fcl-passrc\src\pparser.pas',
  pasresolveeval in '..\Compiler\packages\fcl-passrc\src\pasresolveeval.pas',
  pasuseanalyzer in '..\Compiler\packages\fcl-passrc\src\pasuseanalyzer.pas',
  avl_tree in '..\Compiler\packages\fcl-base\src\avl_tree.pas',
  Pas2JS.Compiler.Delphi in '..\Pas2JS.Compiler.Delphi.pas',
  Pas2JS.Compiler.Options.Form in '..\Pas2JS.Compiler.Options.Form.pas' {CompilerOptionsForm},
  Debug.Main in 'Debug.Main.pas' {DebugMain};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDebugMain, DebugMain);
  Application.Run;
end.
