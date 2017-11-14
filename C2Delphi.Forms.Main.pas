unit C2Delphi.Forms.Main;

interface

{$DEFINE USE_DELPHIAST}
{$DEFINE USE_DWS}

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Actions,


  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.WinXCtrls,
  Vcl.ComCtrls,
  Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnMan,
  Vcl.Menus,

  Vcl.Styles.Hooks,
  Threading,
  SynEditKeyCmds,

{$IFDEF USE_DELPHIAST}
  DelphiAST,
  DelphiAST.Consts,
  DelphiAST.Classes,
{$ENDIF}

{$IFDEF USE_DWS}
  dwsComp,
  dwsExprs,
  dwsStringResult,
{$ENDIF}

  WvN.Pascal.Model,
  WvN.Pascal.CReader, Vcl.AppEvnts,
  SynHighlighterPas, SynEditHighlighter, SynHighlighterCpp, SynEdit,

  Vcl.Themes

  ;


const
  AppName='C to Delphi';

type
  TfrmMain = class(TForm)
    Splitter1: TSplitter;
    SearchBox1: TSearchBox;
    TreeView1: TTreeView;
    Splitter2: TSplitter;
    ActionManager1: TActionManager;
    Action1: TAction;
    actRun: TAction;
    Splitter3: TSplitter;
    ListBox1: TListBox;
    StatusBar1: TStatusBar;
    PopupMenu1: TPopupMenu;
    Run1: TMenuItem;
    ProgressBar1: TProgressBar;
    ApplicationEvents1: TApplicationEvents;
    Panel1: TPanel;
    Panel2: TPanel;
    StatusBar2: TStatusBar;
    StatusBar3: TStatusBar;
    edCCode: TSynEdit;
    edPascalCode: TSynEdit;
    SynCppSyn1: TSynCppSyn;
    SynPasSyn1: TSynPasSyn;
    procedure FormCreate(Sender: TObject);
    procedure edCCodeSelectionChanged(Sender: TObject);
    procedure  edCCodeChange(Sender:TObject);
    procedure SearchBox1Change(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure Action1Execute(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure edPascalCodeCaretChanged(ASender: TObject; X, Y: Integer);
    procedure edCCodeCaretChanged(ASender: TObject; X, Y: Integer);
    procedure ListBox1DblClick(Sender: TObject);
    procedure edPascalCodeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ApplicationEvents1Hint(Sender: TObject);
    procedure ApplicationEvents1Exception(Sender: TObject; E: Exception);
    procedure FormDestroy(Sender: TObject);
    procedure edPascalCodeClick(Sender: TObject);
    procedure edCCodeSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure edPascalCodeSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure TreeView1CustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    procedure WMDROPFILES(var msg : TWMDropFiles) ; message WM_DROPFILES;
    procedure UpdateTree;
    procedure ShowElement(Sender:TObject; el: TPascalElement);
    function ReadCCodeFromFile(cfn: string):string;
    procedure GetRangeSource(const sl: TStrings; e: TPascalElement; out p1,p2: TBufferCoord);
    procedure GetRangeRender(const sl: TStrings; e: TPascalElement; out p1,p2: TBufferCoord);
    procedure FindParent(var e: TPascalElement);
  public
    e:TPascalElement;
    CFileName:string;
    Pas:WvN.Pascal.Model.TPascalUnit;

{$IFDEF USE_DWS}
    dws : TDelphiWebScript;
    prog : IdwsProgram;
    exec : IdwsProgramExecution;
{$ENDIF}

{$IFDEF USE_DELPHIAST}
    ex : ESyntaxTreeException;
{$ENDIF}
    task:ITask;
    procedure Run;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses ShellAPI, Math, System.IOUtils, dwsErrors,
  System.Diagnostics,
  RegularExpressions
;


{$IFDEF USE_DELPHIAST}
function Parse(const FileName: string;var ex:ESyntaxTreeException): string;
var
  SyntaxTree: TSyntaxNode;
begin
  try
    ex := nil;
    SyntaxTree := TPasSyntaxTreeBuilder.Run(FileName, False);
    SyntaxTree.Free;
    Result := 'Pascal syntax OK';
  except
    on E: ESyntaxTreeException do
    begin
      Result := Format('[%d, %d] %s', [E.Line, E.Col, E.Message]);
      ex := E;
    end;
  end;
end;
{$ENDIF}


function getPosition(const sl:TStrings; Offset:Integer):TBufferCoord;
var
  I,n: Integer;
const lineEndSize=length(sLineBreak);
begin
  n := 0;
  for I := 0 to sl.Count-1 do
  begin
    if (n + sl[I].Length + lineEndSize) > offset then
    begin
      Result.Line := I;
      Result.Char := Offset - n;
      Exit;
    end;
    n := n + sl[I].Length + lineEndSize;
  end;
  Result.Line := 0;
  Result.Char := 0;
end;


procedure TfrmMain.edCCodeSelectionChanged(Sender: TObject);
var c,t:string;
begin
  if edCCode.SelLength=0 then
  begin
    edCCodeCaretChanged(edCCode, edCCode.CaretX, edCCode.CaretY);
    Exit;
  end;

  c := edCCode.SelText;
  pas := TPascalUnit.Create(nil);

  c_to_pas(c,t,pas.Name,
         procedure(progress:double;const text:string)
         begin
           TThread.Synchronize(TThread.CurrentThread,
           procedure
           begin
             ProgressBar1.Position := round(Progress * 100);
             Statusbar1.Panels[2].Text := round(Progress * 100).toString+'% '+ Text;
             if ListBox1.Count>0 then
               ListBox1.Perform(lb_SetTopIndex,ListBox1.Count-1,0);
           end);
         end,
         Pas);
  UpdateTree;

  edPascalCode.Text := pas.toPascal;
end;

procedure TfrmMain.edCCodeSpecialLineColors(Sender: TObject; Line: Integer;
  var Special: Boolean; var FG, BG: TColor);
var p1,p2:TBufferCoord;
begin
  if e=nil then
    Exit;

  if e.Sourceinfo.Position>edCCode.Text.Length then
    Exit;

  if e.Sourceinfo.Position<0 then
    Exit;

  p1 := getPosition( edCCode.Lines, e.Sourceinfo.Position);
  p2 := getPosition( edCCode.Lines, e.Sourceinfo.Position + e.Sourceinfo.Length );
  if InRange(Line, p1.Line, p2.Line) then
  begin
    Special := False;
    BG := edCCode.Color + $111111;
  end;

end;

procedure TfrmMain.GetRangeSource(const sl: TStrings; e: TPascalElement; out p1,p2: TBufferCoord);
begin
  p1 := getPosition(sl, e.Sourceinfo.Position);
  p2 := getPosition(sl, e.Sourceinfo.Position + e.Sourceinfo.Length);
end;

procedure TfrmMain.GetRangeRender(const sl: TStrings; e: TPascalElement; out p1,p2: TBufferCoord);
begin
  p1 := getPosition(sl, e.Renderinfo.Position);
  p2 := getPosition(sl, e.Renderinfo.Position + e.Renderinfo.Length);
end;


procedure TfrmMain.edPascalCodeCaretChanged(ASender: TObject; X, Y: Integer);
var c:TClassdef;e:TPascalElement;
  p1,p2:TBufferCoord;
begin
  StatusBar2.Panels[0].Text := Format('[Line:%d,Col%d]', [Y,X]);
  if pas=nil then
    Exit;


  for c in pas.Classes do
    for e in c.FMethods do
    begin
      GetRangeRender(edPascalCode.Lines, e, p1, p2);
      if InRange(Y,p1.Line, p2.Line) then
      begin
        ShowElement(ASender,e);
        edCCode.Refresh;
        Exit;
      end;
    end;

  for e in pas.GlobalArrays1D do
  begin
    GetRangeRender(edPascalCode.Lines, e, p1, p2);
    if InRange(Y,p1.Line, p2.Line) then
    begin
      ShowElement(ASender,e);
      edCCode.Refresh;
      Exit;
    end;
  end;

  for e in pas.GlobalArrays2D do
  begin
    GetRangeRender(edPascalCode.Lines, e, p1, p2);
    if InRange(Y,p1.Line, p2.Line) then
    begin
      ShowElement(ASender,e);
      edCCode.Refresh;
      Exit;
    end;
  end;

  for e in pas.Classes do
  begin
    GetRangeRender(edPascalCode.Lines, e, p1, p2);
    if InRange(Y,p1.Line, p2.Line) then
    begin
      ShowElement(ASender,e);
      edCCode.Refresh;
      Exit;
    end;
  end;

end;

procedure TfrmMain.edPascalCodeChange(Sender: TObject);
{$IFDEF USE_DELPHIAST}
var fn,t:string;
{$ENDIF}
begin
{$IFDEF USE_DELPHIAST}
  fn := TPath.Combine(TPath.GetTempPath, Pas.Name+'_tmp.pas');
  edPascalCode.Lines.SaveToFile(fn);
  t := Parse(fn,ex);
  ListBox1.Items.Add(t);
  TFile.Delete(fn);
{$ENDIF}
end;


procedure TfrmMain.edPascalCodeClick(Sender: TObject);
begin
  edPascalCodeCaretChanged(edPascalCode, edPascalCode.CaretX, edPascalCode.CaretY);
end;

procedure TfrmMain.edPascalCodeSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
var p1,p2:TBufferCoord;
begin
  if e=nil then
    Exit;

  p1 := getPosition( edPascalCode.Lines, e.Renderinfo.Position);
  p2 := getPosition( edPascalCode.Lines, e.Renderinfo.Position + e.Renderinfo.Length );
  if InRange(Line, p1.Line, p2.Line) then
  begin
    Special := False;
    BG := edPascalCode.Color + $111111;
  end;

end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(Task) then
    Task.Cancel;
  Task.Wait(1);

end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  TStyleManager.Engine.RegisterStyleHook(TCustomSynEdit, TScrollingStyleHook);

{$IFDEF USE_DWS}
  dws:=TDelphiWebScript.Create(nil);
{$ENDIF}
  CFileName := '';
  edCCode.Color := $00222827;
  edCCode.Text :=
        'void hello(int x){'+sLineBreak+
        '    printf("Hello world %d\n",x);'+sLineBreak+
        '}'+sLineBreak+
        ''+sLineBreak+
        '/* '+sLineBreak+
        '  Multiline'+sLineBreak+
        '  Comment'+sLineBreak+
        '*/ '+sLineBreak+
        'int main(){'+sLineBreak+
        '  for(int i=0;i<=10;i++){'+sLineBreak+
        '    hello(i);'+sLineBreak+
        '  } '+sLineBreak+
        '}'
    ;


  DragAcceptFiles( Handle, True ) ;
  edCCodeChange(nil);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
{$IFDEF USE_DWS}
  dws.Free;
{$ENDIF}
{$IFDEF USE_DELPHIAST}
  ex.Free;
{$ENDIF}
  pas.Free;
end;

procedure TfrmMain.ListBox1DblClick(Sender: TObject);
var line,Col:integer;m:TMatch; pos:TBufferCoord;
begin
  if ListBox1.ItemIndex<0 then Exit;


  m := TRegEx.Match(ListBox1.Items[ListBox1.ItemIndex], '\[(?<Line>\d+)\,\s*(?<Col>\d+)\]\s*(?<Message>.*)');
  if not m.Success then
    Exit;

  Line := StrToInt(m.Groups['Line'].Value)-1;
  Col := StrToInt(m.Groups['Col'].Value);

  pos.Char := Col;
  pos.Line := Line;

  edPascalCode.SelStart := edPascalCode.RowColToCharIndex(Pos);
  Pos.Char := Pos.Char + 1;
  edPascalCode.SelEnd := edPascalCode.RowColToCharIndex(Pos);

  edPascalCode.TopLine := Line - 3;
  edPascalCode.SetFocus;
  FocusControl(edPascalCode);

end;

{$IFDEF USE_DWS}
procedure TfrmMain.Run;
var s,code:string;
begin
  code := edPascalCode.Text;

  // fix some common problems.

  // Somehow DWS uses PintLn instead of WriteLn
  code := code.Replace('WriteLn','PrintLn',[ rfIgnoreCase, rfReplaceAll ]);
  code := code.Replace('Write','Print',[ rfIgnoreCase, rfReplaceAll ]);

  // compile
  prog:=dws.Compile(code);

  ListBox1.Clear;
  ListBox1.Items.Add('Output:');
  if prog.Msgs.Count=0 then
  begin
    try
     // run
     exec := prog.Execute;

     // show program output in listbox
     for s in exec.Result.ToString.Split([sLineBreak]) do
       ListBox1.Items.Add(s);
    except
      on E: Exception do
        ListBox1.Items.Add(E.ClassName+': '+E.Message);
    end;
  end
  else
  begin
    ListBox1.Items.Add(prog.Msgs.AsInfo);
  end;
end;

{$ELSE}
procedure TfrmMain.Run;
begin
  // include DWS to run the generated code
end;
{$ENDIF}


function TfrmMain.ReadCCodeFromFile(cfn: string): string;
var
  hfn: string;
begin
  // try to open the header file too...

  Result := TFile.ReadAllText(cfn);
  if not SameText(ExtractFileExt(cfn), '.h') then
  begin
    hfn := ChangeFileExt(cfn, '.h');
    if TFile.Exists(hfn) then
      Result := TFile.ReadAllText(hfn) + sLineBreak + Result;
  end;
end;

procedure TfrmMain.FindParent(var e: TPascalElement);
begin
  repeat
    if e.Renderinfo.Length > 1 then
      break
    else
      e := e.Owner;
  until (e = nil) or (e.Owner = nil);
end;

procedure TfrmMain.ShowElement(Sender:TObject; el: TPascalElement);
var
  p1,p3: TBufferCoord;
begin
  StatusBar1.Panels[4].Text := Format('%s (%s)',[el.Name,el.Name.TrimLeft(['T'])]);

  e := el;
  FindParent(e);
  if e <> nil then
  begin
    p1 := getPosition(edCCode.Lines, e.Sourceinfo.Position);
    while edCCode.Lines[p1.Line].Trim = '' do
    begin
      p1.Line := p1.Line + 1;
      if p1.Line > edCCode.Lines.Count then
        Break;
    end;
    if Sender<>edCCode then
    begin
      edCCode.TopLine   := p1.Line;
      edCCode.Refresh;
    end;
  end;

  e := el;
  FindParent(e);

  if e<>nil then
  begin
    p3 := getPosition(edPascalCode.Lines, e.Renderinfo.Position);
    if Sender<>edPascalCode then
    begin
      edPascalCode.TopLine   := p3.Line - (p1.Line - edCCode.TopLine + 1);
      edPascalCode.Refresh;
    end;
  end;
end;



procedure TfrmMain.SearchBox1Change(Sender: TObject);
begin
  edCCode.SearchEngine.FindAll(SearchBox1.Text);
  Caption := Format('Matches:%d', [edCCode.SearchEngine.ResultCount]);;
end;

procedure TfrmMain.TreeView1Change(Sender: TObject; Node: TTreeNode);
var el:TPascalElement;
begin
  if Node.Data=nil then
    Exit;

  el := Node.Data;
  if el=nil then
    Exit;
  ShowElement(Sender,el);
end;

procedure TfrmMain.TreeView1CustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var el:TPascalElement;
begin
  el := TPascalElement(Node.Data);

  if el is TClassDef     then Sender.Canvas.Font.Color := $FF9999;
  if el is TVariableList then Sender.Canvas.Font.Color := $99CC99;
  if el is TVariable     then Sender.Canvas.Font.Color := $999999;
  if el is TPascalUnit   then Sender.Canvas.Font.Color := $888888;
  if el is TRoutine      then Sender.Canvas.Font.Color := $6666FF;
  if el is TUsesList     then Sender.Canvas.Font.Color := $669999;
  if el is TUsesListItem then Sender.Canvas.Font.Color := $66cccc;
  if el is TEnumDef      then Sender.Canvas.Font.Color := $66ffcc;
  if el is TArrayDef1D   then Sender.Canvas.Font.Color := $ffffcc;
  if el is TArrayDef2D   then Sender.Canvas.Font.Color := $ffccff;

  DefaultDraw := True;
end;

procedure TfrmMain.WMDROPFILES(var msg: TWMDropFiles);
const
  MAXFILENAME = 255;
var
  cnt, fileCount: integer;
  code,cfn,t:string;
  fileName : array [0 .. MAXFILENAME] of char;
  p:TPascalUnit;
begin
  fileCount := DragQueryFile(msg.Drop, $FFFFFFFF, fileName, MAXFILENAME);
  for cnt := 0 to fileCount -1 do
  begin
    DragQueryFile(msg.Drop, cnt, fileName, MAXFILENAME);
    cfn := fileName;
    if fileCount>1 then
    begin
      p := TPascalUnit.Create(nil);
      try
      c_to_pas(ReadCCodeFromFile(cfn),t,changefileext(ExtractFilename(cfn),''),
         procedure(progress:double;const text:string)
         begin
           TThread.Synchronize(TThread.CurrentThread,
           procedure
           begin
             ProgressBar1.Position := round(Progress * 100);
             Statusbar1.Panels[2].Text := round(Progress * 100).toString+'% '+ Text;
//             ListBox1.Items.Add(Statusbar1.Panels[2].Text );
             if ListBox1.Count>0 then
               ListBox1.Perform(lb_SetTopIndex,ListBox1.Count-1,0);
           end);
         end,
         p
      );
      TFile.WriteAllText( ChangeFileExt(fileName,'.pas'), p.toPascal);
      finally
        p.Free;
      end;
    end;
  end;

  if fileCount>0 then
  begin
    code := ReadCCodeFromFile(cfn);
    if length(code)>1024*150 then
    begin
      edCCode.Highlighter := nil;
    end
    else
    begin
      edCCode.Highlighter := SynCppSyn1;
      edCCode.Color := $00222827;
    end;

    edCCode.Clear;
    edCCode.Text := code;
    edCCode.Hint := ChangeFileExt(ExtractFileName(cfn),'');
    CFileName := fileName;
    pas.Name := changefileext(ExtractFilename(cfn),'');
    edCCodeChange(nil);
  end;

  DragFinish(msg.Drop);
end;

procedure TfrmMain.UpdateTree;
  procedure AddNode(p:TPascalElement;t:TTreeNode);
  var i:integer;tn:TTreeNode;
  begin
    tn := TreeView1.Items.AddChild(t, p.ToString );
    tn.Data := p;

    for I := 0 to p.Count-1 do
      if p.Children[I].Visible then
      begin
        AddNode(p.Children[I],tn);
      end;
  end;
begin
  TreeView1.Items.BeginUpdate;
  TreeView1.Items.Clear;
  AddNode(pas,nil);

  TreeView1.FullExpand;
  TreeView1.Items.EndUpdate;



end;

procedure TfrmMain.Action1Execute(Sender: TObject);
begin
  edCCode.Lines.SaveToFile(ChangeFileExt(cFileName,'_prep_.c'));
  edPascalCode.Lines.SaveToFile(ChangeFileExt(cFileName,'.pas'));
end;

procedure TfrmMain.actRunExecute(Sender: TObject);
begin
  Run;
end;

procedure TfrmMain.ApplicationEvents1Exception(Sender: TObject; E: Exception);
begin
  ListBox1.Items.Add(E.Message)
end;

procedure TfrmMain.ApplicationEvents1Hint(Sender: TObject);
begin
  StatusBar1.Panels[3].Text := Application.Hint;
end;

procedure TfrmMain.edCCodeCaretChanged(ASender: TObject; X, Y: Integer);
var text:string; c:TClassDef;
e:TPascalElement; p1,p2:TBufferCoord;

begin
  if pas=nil then
    Exit;

  text := edCCode.Text;

  StatusBar3.Panels[0].Text := Format('[Line:%d,Col%d]', [Y,X]);


  for c in pas.Classes do
    for e in c.FMethods do
    begin
      GetRangeSource(edCCode.Lines, e, p1, p2);
      if InRange(Y,p1.Line, p2.Line) then
      begin
        ShowElement(ASender,e);
        Exit;
      end;
    end;

  for e in pas.GlobalArrays1D do
  begin
    GetRangeSource(edCCode.Lines, e, p1, p2);
    if InRange(Y,p1.Line, p2.Line) then
    begin
      ShowElement(ASender,e);
      Exit;
    end;
  end;

  for e in pas.GlobalArrays2D do
  begin
    GetRangeSource(edCCode.Lines, e, p1, p2);
    if InRange(Y,p1.Line, p2.Line) then
    begin
      ShowElement(ASender,e);
      Exit;
    end;
  end;

  for e in pas.Classes do
  begin
    GetRangeSource(edCCode.Lines, e, p1, p2);
    if InRange(Y,p1.Line, p2.Line) then
    begin
      ShowElement(ASender,e);
      Exit;
    end;
  end;

end;

procedure TfrmMain.edCCodeChange(Sender:TObjecT);
var
  tl: Integer;t:string;
  c:string;

begin
  tl := edPascalCode.TopLine;
  c := edCCode.Text;

  if task<>nil then
    if task.Status in [TTaskStatus.Running,TTaskStatus.WaitingToRun] then
    begin
      task.Cancel;
      task.Wait(300);
    end;

  ProgressBar1.Position := 0;
  ProgressBar1.Min := 0;
  ProgressBar1.Max := 100;

  StatusBar1.Panels[1].Text := 'Converting...';
  edPascalCode.Enabled := False;
  edPascalCode.Color := $000033;
  ListBox1.Clear;

  task := TTask.Create(
    procedure
    var p:TPascalUnit; n:string;
    begin
      if pas<>nil then
        n:= pas.Name
      else
        n := 'tmp';

      p := TPascalUnit.Create(nil);
      c_to_pas(c,t,n,
         // callback to report progress
         procedure(progress:double;const text:string)
         begin
           TThread.Synchronize(TThread.CurrentThread,
           procedure
           begin
             if progress < 1 then
             begin
               ProgressBar1.Position := round(Progress * 100);
               Statusbar1.Panels[2].Text := round(Progress * 100).toString+'% '+ Text
             end
             else
             begin
               ProgressBar1.Position := 0;
               Statusbar1.Panels[2].Text := '';
             end;
             if ListBox1.Count>0 then
               ListBox1.Perform(lb_SetTopIndex,ListBox1.Count-1,0);
           end);
         end,
         p);


       TThread.Synchronize(TThread.CurrentThread,
       procedure
       begin
         Pas.Free;

         pas := p;
         frmMain.Caption := Application.Title;
         if pas.Name <> '' then
           frmMain.Caption := p.Name + ' - ' + Caption;

         UpdateTree;
         edPascalCode.Text := pas.toPascal;
         edPascalCode.TopLine := tl;
         edPascalCode.Enabled := True;
//@@@          edPascalCode.OnChange(sender);
         edPascalCode.Color := $00222827;
         StatusBar1.Panels[1].Text := '';
         p := nil;
       end);

    end);

  task.Start;
end;


end.
