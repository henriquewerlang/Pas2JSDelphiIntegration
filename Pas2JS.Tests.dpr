program Pas2JS.Tests;

uses
  Delphi.Helper in 'Helper\Delphi.Helper.pas',
  fpcunit in 'Compiler\packages\fcl-fpcunit\src\fpcunit.pas',
  testregistry in 'Compiler\packages\fcl-fpcunit\src\testregistry.pas',
  testutils in 'Compiler\packages\fcl-fpcunit\src\testutils.pas',
  testdecorator in 'Compiler\packages\fcl-fpcunit\src\testdecorator.pas',
  fppas2js in 'Compiler\packages\pastojs\src\fppas2js.pas',
  jsbase in 'Compiler\packages\fcl-js\src\jsbase.pas',
  jssrcmap in 'Compiler\packages\fcl-js\src\jssrcmap.pas',
  jstoken in 'Compiler\packages\fcl-js\src\jstoken.pas',
  jstree in 'Compiler\packages\fcl-js\src\jstree.pas',
  jswriter in 'Compiler\packages\fcl-js\src\jswriter.pas',
  pasresolveeval in 'Compiler\packages\fcl-passrc\src\pasresolveeval.pas',
  pasresolver in 'Compiler\packages\fcl-passrc\src\pasresolver.pas',
  pastree in 'Compiler\packages\fcl-passrc\src\pastree.pas',
  pasuseanalyzer in 'Compiler\packages\fcl-passrc\src\pasuseanalyzer.pas',
  pparser in 'Compiler\packages\fcl-passrc\src\pparser.pas',
  pscanner in 'Compiler\packages\fcl-passrc\src\pscanner.pas',
  fpjson in 'Compiler\packages\fcl-json\src\fpjson.pas',
  jsonparser in 'Compiler\packages\fcl-json\src\jsonparser.pas',
  jsonreader in 'Compiler\packages\fcl-json\src\jsonreader.pas',
  jsonscanner in 'Compiler\packages\fcl-json\src\jsonscanner.pas',
  tcconverter in 'Compiler\packages\pastojs\tests\tcconverter.pas',
  tcgenerics in 'Compiler\packages\pastojs\tests\tcgenerics.pas',
  tcmodules in 'Compiler\packages\pastojs\tests\tcmodules.pas',
  tcoptimizations in 'Compiler\packages\pastojs\tests\tcoptimizations.pas',
  pas2jsuseanalyzer in 'Compiler\packages\pastojs\src\pas2jsuseanalyzer.pas',
  tcsrcmap in 'Compiler\packages\pastojs\tests\tcsrcmap.pas',
  fppjssrcmap in 'Compiler\packages\pastojs\src\fppjssrcmap.pas',
  pas2jsutils in 'Compiler\packages\pastojs\src\pas2jsutils.pas',
  TestInsight.FPCUnit in 'TestInsight.FPCUnit.pas';

begin
  RunRegisteredTests;
end.
