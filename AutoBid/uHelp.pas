unit uHelp;

interface

uses
  Classes, SysUtils, Windows;


  procedure MouseClick(location: TPoint);
  procedure KeyboardInput(AContent: string);

  procedure ClearContent;
  procedure PasteInput(const AContent: string);

var
  g_KeyIntputInterval : Integer = 100;

implementation

uses
  Vcl.Clipbrd;


const MouseIntputInterval = 100;

procedure MouseClick(location: TPoint);
begin
  Sleep(MouseIntputInterval);
  windows.SetCursorPos(location.x,location.y);
  //µã»÷
  mouse_event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN,0,0,0,0);
  mouse_event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP,0,0,0,0);
end;

procedure InputVk(VK: Word);
begin
  if GetKeyState(VK_CONTROL) < 0 then
    keybd_event(VK_CONTROL,0,KEYEVENTF_KEYUP,0);
  Sleep(g_KeyIntputInterval);
  keybd_event(VK,0,0,0);
  keybd_event(VK,0,KEYEVENTF_KEYUP,0);
end;

procedure KeyboardInput(AContent: string);
var
  I:Integer;
begin
  for I := 1 to Length(AContent) do
  begin
    InputVk(Ord(AContent[I]));
  end;
end;

procedure ClearContent;
begin
  InputVk(VK_HOME);


  InputVk(VK_DELETE);
  InputVk(VK_DELETE);
  InputVk(VK_DELETE);
  InputVk(VK_DELETE);
  InputVk(VK_DELETE);
  InputVk(VK_DELETE);
  InputVk(VK_DELETE);
end;

procedure PasteInput(const AContent: string);
var
  LClip: TClipboard;
begin
  LClip := TClipboard.Create;
  LClip.AsText := AContent;

  if GetKeyState(VK_CONTROL) >= 0 then
    keybd_event(VK_CONTROL,0,0,0);

  keybd_event(Ord('A'),0,0,0);
  keybd_event(Ord('A'),0,KEYEVENTF_KEYUP,0);

  Sleep(g_KeyIntputInterval);
  keybd_event(Ord('V'),0,0,0);
  keybd_event(Ord('V'),0,KEYEVENTF_KEYUP,0);


  if GetKeyState(VK_CONTROL) < 0 then
    keybd_event(VK_CONTROL,0,KEYEVENTF_KEYUP,0);
  LClip.Free;
end;

end.
