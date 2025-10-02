program Pas2JS.Tests;

uses
  Delphi.Helper in 'Compiler\Delphi.Helper.pas',
  fpcunit in 'Pas2JS\compiler\packages\fcl-fpcunit\src\fpcunit.pas',
  testregistry in 'Pas2JS\compiler\packages\fcl-fpcunit\src\testregistry.pas',
  testutils in 'Pas2JS\compiler\packages\fcl-fpcunit\src\testutils.pas',
  testdecorator in 'Pas2JS\compiler\packages\fcl-fpcunit\src\testdecorator.pas',
  fppas2js in 'Pas2JS\compiler\packages\pastojs\src\fppas2js.pas',
  jsbase in 'Pas2JS\compiler\packages\fcl-js\src\jsbase.pas',
  jssrcmap in 'Pas2JS\compiler\packages\fcl-js\src\jssrcmap.pas',
  jstoken in 'Pas2JS\compiler\packages\fcl-js\src\jstoken.pas',
  jstree in 'Pas2JS\compiler\packages\fcl-js\src\jstree.pas',
  jswriter in 'Pas2JS\compiler\packages\fcl-js\src\jswriter.pas',
  pasresolveeval in 'Pas2JS\compiler\packages\fcl-passrc\src\pasresolveeval.pas',
  pasresolver in 'Pas2JS\compiler\packages\fcl-passrc\src\pasresolver.pas',
  pastree in 'Pas2JS\compiler\packages\fcl-passrc\src\pastree.pas',
  pasuseanalyzer in 'Pas2JS\compiler\packages\fcl-passrc\src\pasuseanalyzer.pas',
  pparser in 'Pas2JS\compiler\packages\fcl-passrc\src\pparser.pas',
  pscanner in 'Pas2JS\compiler\packages\fcl-passrc\src\pscanner.pas',
  fpjson in 'Pas2JS\compiler\packages\fcl-json\src\fpjson.pas',
  jsonparser in 'Pas2JS\compiler\packages\fcl-json\src\jsonparser.pas',
  jsonreader in 'Pas2JS\compiler\packages\fcl-json\src\jsonreader.pas',
  jsonscanner in 'Pas2JS\compiler\packages\fcl-json\src\jsonscanner.pas',
  tcconverter in 'Pas2JS\compiler\packages\pastojs\tests\tcconverter.pas',
  tcgenerics in 'Pas2JS\compiler\packages\pastojs\tests\tcgenerics.pas',
  tcmodules in 'Pas2JS\compiler\packages\pastojs\tests\tcmodules.pas',
  tcoptimizations in 'Pas2JS\compiler\packages\pastojs\tests\tcoptimizations.pas',
  pas2jsuseanalyzer in 'Pas2JS\compiler\packages\pastojs\src\pas2jsuseanalyzer.pas',
  tcsrcmap in 'Pas2JS\compiler\packages\pastojs\tests\tcsrcmap.pas',
  fppjssrcmap in 'Pas2JS\compiler\packages\pastojs\src\fppjssrcmap.pas',
  pas2jsutils in 'Pas2JS\compiler\packages\pastojs\src\pas2jsutils.pas',
  TestInsight.FPCUnit in 'TestInsight.FPCUnit.pas';

begin
  RunRegisteredTests;
end.
