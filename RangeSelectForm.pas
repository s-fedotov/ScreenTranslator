unit RangeSelectForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons;

type
  TfrmRangeSelect = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    b_startStop: TBitBtn;
    BitBtn2: TBitBtn;
    procedure b_startStopClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRangeSelect: TfrmRangeSelect;

implementation
uses ScreenTranslatorForm;
{$R *.dfm}

procedure TfrmRangeSelect.BitBtn2Click(Sender: TObject);
begin
  Close;
  Form1.appWorkMode := awmRangeDefine;
  Form1.WindowState := wsNormal;
end;

procedure TfrmRangeSelect.b_startStopClick(Sender: TObject);
begin
  if Form1.tm_translation.Enabled then
    begin
      Form1.tm_translation.Enabled := false;
      b_startStop.Caption := 'Start';
    end
  else
    begin
      Form1.tm_translation.Enabled := true;
      b_startStop.Caption := 'Stop';
    end;
end;

procedure TfrmRangeSelect.FormShow(Sender: TObject);
begin
  if Form1.tm_translation.Enabled then
    b_startStop.Caption := 'Start'
  else
    b_startStop.Caption := 'Stop';

end;

end.

