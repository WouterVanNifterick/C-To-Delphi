program C2Delphi;

uses
  FastMM4,
  Vcl.Forms,
  C2Delphi.Forms.Main in 'C2Delphi.Forms.Main.pas' {frmMain},
  WvN.Pascal.Model in 'WvN.Pascal.Model.pas',
  Vcl.Themes,
  Vcl.Styles,
  WvN.Pascal.CReader in 'WvN.Pascal.CReader.pas',
  Vcl.PlatformVclStylesActnCtrls in '..\..\lib\vcl-styles-utils\Common\Vcl.PlatformVclStylesActnCtrls.pas',
  Vcl.Styles.ColorTabs in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.ColorTabs.pas',
  Vcl.Styles.ControlColor in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.ControlColor.pas',
  Vcl.Styles.Ext in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.Ext.pas',
  Vcl.Styles.Fixes in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.Fixes.pas',
  Vcl.Styles.FormStyleHooks in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.FormStyleHooks.pas',
  Vcl.Styles.Hooks in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.Hooks.pas',
  Vcl.Styles.NC in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.NC.pas',
  Vcl.Styles.OwnerDrawFix in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.OwnerDrawFix.pas',
  Vcl.Styles.Utils in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.Utils.pas',
  Vcl.Styles.Utils.ComCtrls in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.Utils.ComCtrls.pas',
  Vcl.Styles.Utils.Forms in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.Utils.Forms.pas',
  Vcl.Styles.Utils.Graphics in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.Utils.Graphics.pas',
  Vcl.Styles.Utils.Menus in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.Utils.Menus.pas',
  Vcl.Styles.Utils.ScreenTips in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.Utils.ScreenTips.pas',
  Vcl.Styles.Utils.Shadow in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.Utils.Shadow.pas',
  Vcl.Styles.Utils.StdCtrls in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.Utils.StdCtrls.pas',
  Vcl.Styles.Utils.SysControls in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.Utils.SysControls.pas',
  Vcl.Styles.Utils.SysStyleHook in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.Utils.SysStyleHook.pas',
  Vcl.Styles.Utils.SystemMenu in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.Utils.SystemMenu.pas',
  Vcl.Styles.UxTheme in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.UxTheme.pas',
  Vcl.Styles.WebBrowser in '..\..\lib\vcl-styles-utils\Common\Vcl.Styles.WebBrowser.pas',
  DelphiAST in '..\DelphiAST\Source\DelphiAST.pas',
  DelphiAST.Classes in '..\DelphiAST\Source\DelphiAST.Classes.pas',
  DelphiAST.Consts in '..\DelphiAST\Source\DelphiAST.Consts.pas',
  SimpleParser in '..\DelphiAST\Source\SimpleParser\SimpleParser.pas',
  SimpleParser.Lexer in '..\DelphiAST\Source\SimpleParser\SimpleParser.Lexer.pas',
  SimpleParser.Lexer.Types in '..\DelphiAST\Source\SimpleParser\SimpleParser.Lexer.Types.pas',
  SimpleParser.Types in '..\DelphiAST\Source\SimpleParser\SimpleParser.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'C to Delphi';
  TStyleManager.TrySetStyle('Charcoal Dark Slate');
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
