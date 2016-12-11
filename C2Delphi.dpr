program C2Delphi;

uses
  Vcl.Forms,
  C2Delphi.Forms.Main in 'C2Delphi.Forms.Main.pas' {frmMain},
  WvN.Pascal.Model in 'WvN.Pascal.Model.pas',
  Vcl.Themes,
  Vcl.Styles,
  WvN.Pascal.CReader in 'WvN.Pascal.CReader.pas',
  DelphiAST.Classes in 'C:\dev\lib\DelphiAST\Source\DelphiAST.Classes.pas',
  DelphiAST.Consts in 'C:\dev\lib\DelphiAST\Source\DelphiAST.Consts.pas',
  DelphiAST in 'C:\dev\lib\DelphiAST\Source\DelphiAST.pas',
  DelphiAST.Writer in 'C:\dev\lib\DelphiAST\Source\DelphiAST.Writer.pas',
  SimpleParser.Lexer in 'C:\dev\lib\DelphiAST\Source\SimpleParser\SimpleParser.Lexer.pas',
  SimpleParser.Lexer.Types in 'C:\dev\lib\DelphiAST\Source\SimpleParser\SimpleParser.Lexer.Types.pas',
  SimpleParser in 'C:\dev\lib\DelphiAST\Source\SimpleParser\SimpleParser.pas',
  SimpleParser.Types in 'C:\dev\lib\DelphiAST\Source\SimpleParser\SimpleParser.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Charcoal Dark Slate');
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
