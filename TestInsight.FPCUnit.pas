unit TestInsight.FPCUnit;

interface

{$O+,W-}
{$IFDEF FPC}
  {$MODE DELPHI}
  {$MACRO ON}
  {$DEFINE CompilerVersion := 21}
  {$DEFINE RTLVersion := 21}
{$ENDIF}

uses
{$IFDEF FPC}
  Classes,
  Generics.Collections,
{$ELSE}
  System.Classes,
  System.Generics.Collections,
{$IFEND}
  FPCUnit,
  TestInsight.Client;

type
  TTestInsightListener = class(TInterfacedObject, ITestListener)
  private
    FClient: TTestInsightRestClient;

    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
  public
    constructor Create(const baseUrl: string);
    destructor Destroy; override;
  end;

procedure RunRegisteredTests(const baseUrl: string = DefaultUrl);

implementation

uses
  DUnitConsts,
{$IFDEF FPC}
  Rtti,
  StrUtils,
  SysUtils,
  TypInfo;
{$ELSE}
  System.Rtti,
  System.StrUtils,
  System.SysUtils,
  System.TypInfo;
{$IFEND}

procedure RunRegisteredTests(const baseUrl: string);
begin
  var Listener := TTestInsightListener.Create(baseUrl) as ITestListener;
  var Results := TTestResult.Create;

  Results.AddListener(Listener);

  TTestCase.Suite.Run(Results);
end;

{ TTestInsightTestListener }

procedure TTestInsightListener.AddError(ATest: TTest; AError: TTestFailure);
begin

end;

procedure TTestInsightListener.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin

end;

constructor TTestInsightListener.Create(const baseUrl: string);
begin
  inherited Create;

  FClient := TTestInsightRestClient.Create(baseUrl);
//  selectedTests := fClient.GetTests;
//  fSelectedTests := TDictionary<string,Boolean>.Create(Length(selectedTests));
//  for i := 0 to High(selectedTests) do
//    fSelectedTests.Add(selectedTests[i], False);
//  if Length(selectedTests) = 1 then
//    fSelectedTest := selectedTests[0];
//  fSelectTest := Length(selectedTests) <> 0;
//  fOptions := fClient.Options;
end;

destructor TTestInsightListener.Destroy;
begin
end;

procedure TTestInsightListener.EndTest(ATest: TTest);
begin

end;

procedure TTestInsightListener.EndTestSuite(ATestSuite: TTestSuite);
begin

end;

procedure TTestInsightListener.StartTest(ATest: TTest);
begin
  FClient.StartedTesting(ATest.CountTestCases)
end;

procedure TTestInsightListener.StartTestSuite(ATestSuite: TTestSuite);
begin

end;

end.

