unit ScreenTranslator;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Wnd: HWND;
  sName: array[0..256] of char;
  function IsWindowInMyList(NewWnd: HWND): Boolean;
  var
    nI: Integer;
  begin
    Result := false;
    for nI := 0 to pred(ListBox1.Items.Count) do
      begin
        if HWND(ListBox1.Items.Objects[nI]) = NewWnd then
          begin
            Result := true;
            Exit;
          end;
      end;
  end;
  function GetWindowTitle(NewWnd: HWND): Boolean;
  var
    ls: Integer;
  begin
    Result := false;
    if NewWnd = 0 then
      Exit;
    // Only top window
    if GetWindow(NewWnd, GW_OWNER) <> 0 then
      Exit;
    if GetParent(NewWnd) <> 0 then
      Exit;
    ls := GetWindowLong(NewWnd, GWL_STYLE);
    if ls and WS_CHILD <> 0 then
      Exit;
    if GetWindowText(NewWnd, sName, 255) = 0 then
      Exit;
    Result := true;
  end;

begin
  ListBox1.Clear;
  Wnd := GetTopWindow(0);
  repeat
    if not IsWindowInMyList(Wnd) then
      begin
        if (GetWindowTitle(Wnd) and IsWindowVisible(Wnd)) then
          ListBox1.Items.AddObject(StrPas(sName), TObject(Wnd));
      end;
    Wnd := GetNextWindow(Wnd, GW_HWNDNEXT);
  until (Wnd = 0);

end;

end.

