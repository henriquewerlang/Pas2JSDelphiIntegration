unit Delphi.Helper;

interface

uses System.SysUtils, System.Generics.Collections, System.Classes, System.WideStrUtils, System.Generics.Defaults, System.Types, Winapi.Windows;

type
  CodePointer = Pointer;
  DWord = Cardinal;
  EFOpenError = Exception;
  PCodePointer = PPointer;
  PtrInt = Integer;
  PtrUInt = Cardinal;
  PUnicodeChar = PChar;
  QWord = UInt64;
  SizeInt = NativeInt;
  TJSString = String;
  TStringArray = TArray<String>;

  TListFreePascal = class(TList)
  public
    procedure AddRange(const List: TArray<Pointer>); overload;
    procedure AddRange(const List: TList); overload;
  end;

  TFPList = TListFreePascal;

  TFPObjectList = class(TListFreePascal)
  private
    FDestroyValues: Boolean;

    function GetItem(const Index: Integer): TObject;
    procedure SetItem(const Index: Integer; const Value: TObject);
  public
    constructor Create(const DestroyValues: Boolean = False);

    destructor Destroy; override;

    property Items[const Index: Integer]: TObject read GetItem write SetItem; default;
  end;

  THashDictionary<V: class> = class(TObjectDictionary<String, V>)
  private
    function GetItem(const Index: Integer): V;

    procedure SetItem(const Index: Integer; const Value: V);
  public
    constructor Create(const AValue: Boolean = False);

    function Add(const Name: String; const Value: V): Integer;
    function Find(const Name: String): V;
    function FindIndexOf(const Name: String): Integer;
    function IndexOf(const Value: V): Integer;
    function NameOfIndex(const Index: Integer): String;

    procedure Delete(const Index: Integer);
    procedure Extract(const Value: TObject);
    procedure ForEachCall(const Proc: TProc<Pointer, Pointer>; const ParamValue: Pointer);
    procedure Remove(const Value: V);

    property Items[const Index: Integer]: V read GetItem write SetItem; default;
  end;

  THandleStream = class(System.Classes.THandleStream)
  protected
    function GetPosition: Int64; virtual; abstract;

    procedure FakeSeekForward(Offset: NativeInt; const Origin: TSeekOrigin; const Pos: NativeInt);
    procedure InvalidSeek; virtual; abstract;
    procedure ReadNotImplemented;
    procedure WriteNotImplemented;
  end;

  TFPHashObjectList = THashDictionary<TObject>;
  TFPHashList = THashDictionary<TObject>;

const
  AllowDirectorySeparators: TSysCharSet = ['\', '/'];
  AllFilesMask = '*.*';
  LineEnding = sLineBreak;
  DirectorySeparator = '\';
  PathSeparator = '\';
  MAX_PATH = MAX_PATH;

var
  StdErr: THandle;
  StdInputHandle: THandle;

function BoolToStr(const Value: Boolean): String; overload;
function BoolToStr(const Value: Boolean; const TrueValue, FalseValue: String): String; overload;
function EncodeStringBase64(const Value: String): String;
function ExtractWord(N: Integer; const S: String; const WordDelims: TSysCharSet): String;
function ExtractWordPos(N: Integer; const S: String; const WordDelims: TSysCharSet; out Pos: Integer): String;
function GetLastOSError: Cardinal;
function HexStr(const Value: Int64; const Digits: Integer): String; overload;
function HexStr(const Value: Integer): String; overload;
function IsValidIdent(const Ident: string; AllowDots: Boolean = False; StrictDots: Boolean = False): Boolean;
function LeftStr(const Str: String; const Count: Integer): String;
function RightStr(const Str: String; const Count: Integer): String;
function SetDirSeparators(const FileName: String): String;
function SplitCommandLine(S: String): TStringDynArray;
function StringInList(const Value: String; const Values: TArray<String>): Boolean;
function StrToQWord(const Value: String): QWord;
function TryStringToGUID(const Value: String; var GUID: TGUID): Boolean;
function TryStrToQWord(const Str: String; var Value: QWord): Boolean;
function WordCount(const S: String; const WordDelims: TSysCharSet): SizeInt;
function WordPosition(const N: Integer; const S: String; const WordDelims: TSysCharSet): SizeInt;

procedure DoDirSeparators(var FileName: String);
procedure FillByte(var Dest; Count: NativeInt; Value: Byte);
procedure SetCodePage(S: RawByteString; CodePage: Word; Convert: Boolean);

implementation

uses System.NetEncoding;

function WordPosition(const N: Integer; const S: String; const WordDelims: TSysCharSet): SizeInt;
var
  PS, P, PE, Count: Integer;

begin
  Result := 0;
  Count := 0;
  PS := 1;
  PE := Length(S);
  P := PS;
  while (P <= PE) and (Count <> N) do
  begin
    while (P <= PE) and CharInSet(S[P], WordDelims) do
      Inc(P);

    if (P <= PE) then
      Inc(Count);

    if (Count <> N) then
      while (P <= PE) and not CharInSet(S[P], WordDelims) do
        Inc(P)
    else
      Result := (P - PS) + 1;
  end;
end;

function WordCount(const S: String; const WordDelims: TSysCharSet): SizeInt;
var
  P, L: Integer;

begin
  Result := 0;
  P := 1;
  L := Length(S);
  while P <= L do
  begin
    while (P <= L) and CharInSet(S[P], WordDelims) do
      Inc(P);

    if (P <= L) then
      Inc(Result);

    while (P <= L) and not CharInSet(S[P], WordDelims) do
      Inc(P);
  end;
end;

function ExtractWord(N: Integer; const S: String; const WordDelims: TSysCharSet): String;
var
  i: LongInt;

begin
  Result := ExtractWordPos(N, S, WordDelims, i);
end;

function ExtractWordPos(N: Integer; const S: String; const WordDelims: TSysCharSet; out Pos: Integer): String;

var
  i, j, L: SizeInt;

begin
  j := 0;
  i := WordPosition(N, S, WordDelims);
  if (i > MaxInt) then
  begin
    Result := '';
    Pos := -1;
    Exit;
  end;
  Pos := i;
  if (i <> 0) then
  begin
    j := i;
    L := Length(S);
    while (j <= L) and not CharInSet(S[j], WordDelims) do
      Inc(j);
  end;
  Result := Copy(S, i, j - i);
end;

function SplitCommandLine(S: String): TStringDynArray;

  function GetNextWord: String;
  const
    WhiteSpace = [' ', #9, #10, #13];
    Literals = ['"', ''''];

  var
    Wstart, wend: Integer;
    InLiteral: Boolean;
    LastLiteral: Char;

    procedure AppendToResult;

    begin
      Result := Result + Copy(S, Wstart, wend - Wstart);
      Wstart := wend + 1;
    end;

  begin
    Result := '';
    Wstart := 1;
    while (Wstart <= Length(S)) and CharInSet(S[Wstart], WhiteSpace) do
      Inc(Wstart);
    wend := Wstart;
    InLiteral := False;
    LastLiteral := #0;
    while (wend <= Length(S)) and (Not CharInSet(S[wend], WhiteSpace) or InLiteral) do
    begin
      if CharInSet(S[wend], Literals) then
        If InLiteral then
        begin
          InLiteral := Not(S[wend] = LastLiteral);
          if not InLiteral then
            AppendToResult;
        end
        else
        begin
          InLiteral := True;
          LastLiteral := S[wend];
          AppendToResult;
        end;
      Inc(wend);
    end;
    AppendToResult;
    while (wend <= Length(S)) and CharInSet(S[wend], WhiteSpace) do
      Inc(wend);
    Delete(S, 1, wend - 1);
  end;

var
  W: String;
  len: Integer;

begin
  len := 0;
  Result := nil;
  SetLength(Result, (Length(S) div 2) + 1);

  while Length(S) > 0 do
  begin
    W := GetNextWord;

    if W <> '' then
    begin
      Result[len] := W;

      Inc(len);
    end;
  end;
  SetLength(Result, len);
end;

procedure DoDirSeparators(var FileName: String);
begin

end;

function TryStringToGUID(const Value: String; var GUID: TGUID): Boolean;
begin
  try
    GUID := StringToGUID(Value);
    Result := True;
  except
    Result := False;
  end;
end;

procedure FillByte(var Dest; Count: NativeInt; Value: Byte);
begin
  FillChar(Dest, Count, Value);
end;

function EncodeStringBase64(const Value: String): String;
begin
  var
  Base64 := TBase64Encoding.Create(0);

  Result := Base64.Encode(Value);

  Base64.Free;
end;

function IsValidIdent(const Ident: string; AllowDots, StrictDots: Boolean): Boolean;
begin
  Result := System.SysUtils.IsValidIdent(Ident, AllowDots);
end;

function GetLastOSError: Cardinal;
begin
  Result := GetLastError;
end;

function HexStr(const Value: Integer): String;
begin
  Result := IntToHex(Value);
end;

function HexStr(const Value: Int64; const Digits: Integer): String;
begin
  Result := IntToHex(Value, Digits);
end;

procedure SetCodePage(S: RawByteString; CodePage: Word; Convert: Boolean);
var
  SS: RawByteString absolute S;

begin
  System.SetCodePage(SS, CodePage, Convert);
end;

function BoolToStr(const Value: Boolean): String;
begin
  Result := System.SysUtils.BoolToStr(Value);
end;

function BoolToStr(const Value: Boolean; const TrueValue, FalseValue: String): String;
begin
  if Value then
    Result := TrueValue
  else
    Result := FalseValue;
end;

function SetDirSeparators(const FileName: String): String;
begin
  Result := FileName;
end;

function StringInList(const Value: String; const Values: TArray<String>): Boolean;
begin
  Result := False;

  for var V in Values do
    if V = Value then
      Exit(True);
end;

function StrToQWord(const Value: String): QWord;
begin
  Result := StrToUInt64(Value);
end;

function TryStrToQWord(const Str: String; var Value: QWord): Boolean;
begin
  Result := TryStrToUInt64(Str, Value);
end;

function LeftStr(const Str: String; const Count: Integer): String;
begin
  Result := Copy(Str, 1, Count);
end;

function RightStr(const Str: String; const Count: Integer): String;
begin
  Result := Copy(Str, Str.Length - Count, Count);
end;

{ THashDictionary<V> }

function THashDictionary<V>.Add(const Name: String; const Value: V): Integer;
begin
  Result := Count;

  inherited Add(Name, Value);
end;

constructor THashDictionary<V>.Create(const AValue: Boolean);
begin
  if AValue then
    inherited Create([doOwnsValues])
  else
    inherited Create([]);
end;

procedure THashDictionary<V>.Delete(const Index: Integer);
begin
  raise Exception.Create('Not implemented!');
end;

procedure THashDictionary<V>.Extract(const Value: TObject);
begin
  raise Exception.Create('Not implemented!');
end;

function THashDictionary<V>.Find(const Name: String): V;
begin
  if not TryGetValue(Name, Result) then
    Result := Default (V);
end;

function THashDictionary<V>.FindIndexOf(const Name: String): Integer;
begin
  var
  Names := Keys.ToArray;
  Result := -1;

  for var A := Low(Names) to High(Names) do
    if Names[A] = Name then
      Exit(A);
end;

procedure THashDictionary<V>.ForEachCall(const Proc: TProc<Pointer, Pointer>; const ParamValue: Pointer);
begin
  for var Item in Values.ToArray do
    Proc(TObject(Item), ParamValue);
end;

function THashDictionary<V>.GetItem(const Index: Integer): V;
begin
  Result := Values.ToArray[Index];
end;

function THashDictionary<V>.IndexOf(const Value: V): Integer;
begin
  raise Exception.Create('Not implemented!');
end;

function THashDictionary<V>.NameOfIndex(const Index: Integer): String;
begin
  Result := Keys.ToArray[Index];
end;

procedure THashDictionary<V>.Remove(const Value: V);
begin
  for var Pair in Self.ToArray do
    if Pair.Value = Value then
    begin
      inherited Remove(Pair.Key);

      Exit;
    end;

  raise Exception.Create('Item not found!');
end;

procedure THashDictionary<V>.SetItem(const Index: Integer; const Value: V);
begin
  inherited Items[NameOfIndex(Index)] := Value;
end;

{ TFPObjectList }

constructor TFPObjectList.Create(const DestroyValues: Boolean);
begin
  inherited Create;

  FDestroyValues := DestroyValues;
end;

destructor TFPObjectList.Destroy;
begin
  if FDestroyValues then
    for var Value in Self do
      TObject(Value).Free;

  inherited;
end;

function TFPObjectList.GetItem(const Index: Integer): TObject;
begin
  Result := inherited Items[Index];
end;

procedure TFPObjectList.SetItem(const Index: Integer; const Value: TObject);
begin
  inherited Items[Index] := Value;
end;

{ THandleStream }

procedure THandleStream.FakeSeekForward(Offset: NativeInt; const Origin: TSeekOrigin; const Pos: NativeInt);
begin

end;

procedure THandleStream.ReadNotImplemented;
begin
  raise Exception.Create('Not implemented!');
end;

procedure THandleStream.WriteNotImplemented;
begin
  raise Exception.Create('Not implemented!');
end;

{ TListFreePascal }

procedure TListFreePascal.AddRange(const List: TList);
begin
  for var Value in List do
    Add(Value);
end;

procedure TListFreePascal.AddRange(const List: TArray<Pointer>);
begin
  for var Value in List do
    Add(Value);
end;

initialization
  StdErr := GetStdHandle(STD_ERROR_HANDLE);

end.
