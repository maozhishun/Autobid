unit uTestData;

interface

uses
  Classes, SysUtils, DateUtils;

  function GetTestData: string;

implementation

var
  g_TestData: TStringList;
  g_Index: Integer;

function GetTestData: string;
begin
  Result := '';
  if Assigned(g_TestData) and (g_TestData.Count > 0) then
  begin
    if g_Index < g_TestData.Count then
    begin
      Result := g_TestData[g_Index];
      inc(g_Index);
    end;
  end;
end;


initialization
  g_Index := 0;
  g_TestData := TStringList.Create;
  if FileExists('.\data\10b.dat') then
    g_TestData.LoadFromFile('.\data\10b.dat');


finalization
  g_TestData.Free;

end.
