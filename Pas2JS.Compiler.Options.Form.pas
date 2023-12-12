unit Pas2JS.Compiler.Options.Form;

interface

uses Vcl.Forms, Data.DB, Datasnap.DBClient, Vcl.StdCtrls, Vcl.Controls, Vcl.Grids, Vcl.DBGrids, System.Classes, Pas2JS.Compiler.Delphi;

type
  TCompilerOptionsForm = class(TForm)
    lblSearchPath: TLabel;
    SearchPath: TEdit;
    GenerateSingleFile: TCheckBox;
    EnumaratoAsNumber: TCheckBox;
    RemoveNotUsedPrivates: TCheckBox;
    RemoveNotUsedDeclaration: TCheckBox;
    GenerateMapFile: TCheckBox;
    lblSourceRoot: TLabel;
    SourceRootFolder: TEdit;
    lblRelativeSourceFolder: TLabel;
    RelativeSourceFolder: TEdit;
    IncludeSourceInMapFile: TCheckBox;
    AbsoluteFileNames: TCheckBox;
    XXSIProtection: TCheckBox;
    RangeCheckError: TCheckBox;
    IntegerOverflowCheck: TCheckBox;
    CheckObjectsTypeCast: TCheckBox;
    procedure GenerateMapFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    procedure CheckMapFileConfiguration;
  public
    procedure LoadConfiguration(const Compiler: TPas2JSCompilerDelphi);
  end;

implementation

{$R *.dfm}

uses Pas2jsCompiler;

{ TCompilerOptions }

procedure TCompilerOptionsForm.FormCreate(Sender: TObject);
begin
  CheckMapFileConfiguration;
end;

procedure TCompilerOptionsForm.GenerateMapFileClick(Sender: TObject);
begin
  CheckMapFileConfiguration;
end;

procedure TCompilerOptionsForm.LoadConfiguration(const Compiler: TPas2JSCompilerDelphi);
begin
  Compiler.AllJSIntoMainJS := GenerateSingleFile.Checked;
  Compiler.SrcMapBaseDir := RelativeSourceFolder.Text;
  Compiler.SrcMapEnable := GenerateMapFile.Checked;
  Compiler.SrcMapFilenamesAbsolute := AbsoluteFileNames.Checked;
  Compiler.SrcMapInclude := IncludeSourceInMapFile.Checked;
  Compiler.SrcMapSourceRoot := SourceRootFolder.Text;
  Compiler.SrcMapXSSIHeader := XXSIProtection.Checked;

  if IntegerOverflowCheck.Checked then
    Compiler.Options := Compiler.Options + [coOverflowChecks];

  if RangeCheckError.Checked then
    Compiler.Options := Compiler.Options +  [coRangeChecks];

  if CheckObjectsTypeCast.Checked then
    Compiler.Options := Compiler.Options + [coObjectChecks];

  if EnumaratoAsNumber.Checked then
    Compiler.Options := Compiler.Options + [coEnumValuesAsNumbers];

  if not RemoveNotUsedPrivates.Checked then
    Compiler.Options := Compiler.Options + [coKeepNotUsedPrivates];

  if not RemoveNotUsedDeclaration.Checked then
    Compiler.Options := Compiler.Options + [coKeepNotUsedDeclarationsWPO];
end;

procedure TCompilerOptionsForm.CheckMapFileConfiguration;
begin
  AbsoluteFileNames.Enabled := GenerateMapFile.Checked;
  IncludeSourceInMapFile.Enabled := GenerateMapFile.Checked;
  RelativeSourceFolder.Enabled := GenerateMapFile.Checked;
  SourceRootFolder.Enabled := GenerateMapFile.Checked;
  XXSIProtection.Enabled := GenerateMapFile.Checked;
end;

end.
