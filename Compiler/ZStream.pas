unit ZStream;

interface

uses System.Classes, System.ZLib, System.SysUtils, Delphi.Helper;

type
  TCompressionLevel = (clNone, clFastest, clDefault, clMax);

  ECompressionError = class(Exception);
  EDecompressionError = class(Exception);

  TCompressionStream = class(TZCompressionStream)
  public
    constructor Create(const CompressionLevel: TCompressionLevel; const Stream: TStream);

    procedure WriteDWord(const Value: QWord);
  end;

  TDecompressionStream = class(TZDecompressionStream)
  public
    function ReadDWord: QWord;
  end;

implementation

{ TCompressionStream }

constructor TCompressionStream.Create(const CompressionLevel: TCompressionLevel; const Stream: TStream);
begin
  inherited Create(Stream);
end;

procedure TCompressionStream.WriteDWord(const Value: QWord);
begin
  WriteData(Value);
end;

{ TDecompressionStream }

function TDecompressionStream.ReadDWord: QWord;
begin
  ReadData(Result);
end;

end.
