program GenerateNamespaceFile;

{$APPTYPE CONSOLE}

{$R *.res}

uses System.SysUtils, System.IOUtils, System.Generics.Collections, System.Classes;

begin
  try
    var FileSource := TStringList.Create;
    var SourceDirectory := EmptyStr;

    if FindCmdLineSwitch('D', SourceDirectory, True) then
    begin
      var Directories := TDirectory.GetDirectories(SourceDirectory, 'namespaced', TSearchOption.soAllDirectories);

      for var DirectoryName in Directories do
      begin
        var Files := TDirectory.GetFiles(DirectoryName, '*', TSearchOption.soAllDirectories);

        for var FileName in Files do
        begin
          FileSource.LoadFromFile(FileName);

          for var Line in FileSource do
            if Line.StartsWith('{$I ', True) then
            begin
              var FileToFind := Line.Substring(4, Line.Length - 5);

              var NotDottedFiles := TDirectory.GetFiles(DirectoryName + '\..\', FileToFind, TSearchOption.soAllDirectories);

              for var NotDottedFile in NotDottedFiles do
                WriteLn(NotDottedFile);
            end;
        end;
      end;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  Readln;
end.
