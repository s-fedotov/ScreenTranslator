program ScreenTranslator;

uses
  Forms,
  ScreenTranslatorForm in 'ScreenTranslatorForm.pas' {frmScreenTranslator},
  ScreenShot in 'ScreenShot.pas',
  ScreenShotHelper in 'ScreenShotHelper.pas',
  RangeSelectForm in 'RangeSelectForm.pas' {frmRangeSelect},
  StringSimilarity in 'StringSimilarity.pas',
  TextAnalyser in 'TextAnalyser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmScreenTranslator, frmScreenTranslator);
  Application.CreateForm(TfrmRangeSelect, frmRangeSelect);
  Application.Run;
end.
