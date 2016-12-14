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

  BCEditor.Types,
  BCEditor.Editor.Base,
  BCEditor.Editor,
  BCComponent.DragDrop,

  Vcl.Styles.Hooks,

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
  WvN.Pascal.CReader
  ;


const
  AppName='C to Delphi';

type
  TfrmMain = class(TForm)
    BCEditor1: TBCEditor;
    BCEditor2: TBCEditor;
    Splitter1: TSplitter;
    BCDragDrop1: TBCDragDrop;
    BCDragDrop2: TBCDragDrop;
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
    procedure FormCreate(Sender: TObject);
    procedure BCEditor1SelectionChanged(Sender: TObject);
    procedure  BCEditor1Change(Sender:TObject);
    procedure SearchBox1Change(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure Action1Execute(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure BCEditor2CaretChanged(ASender: TObject; X, Y: Integer);
    procedure BCEditor1CaretChanged(ASender: TObject; X, Y: Integer);
    procedure ListBox1DblClick(Sender: TObject);
    procedure BCEditor2Change(Sender: TObject);
  private
    procedure WMDROPFILES(var msg : TWMDropFiles) ; message WM_DROPFILES;
    procedure UpdateTree;
    procedure ShowElement(Sender:TObject; rt: TPascalElement);
    function ReadCCodeFromFile(cfn: string):string;
    procedure GetRangeSource(const text: string; e: TPascalElement; out p1,p2: TBCEditorTextPosition);
    procedure GetRangeRender(const text: string; e: TPascalElement; out p1,p2: TBCEditorTextPosition);
  public
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
    procedure Run;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses ShellAPI, Math, System.IOUtils, BCEditor.Lines, dwsErrors,
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


function getPosition(const s:string; Offset:Integer):tbceditortextposition;
var sl:TStringList;
  I,n: Integer;
const lineEndSize=length(sLineBreak);
begin
  sl := TStringList.Create;
  sl.Text := s;
  try
    n := 0;
    for I := 0 to sl.Count-1 do
    begin
      if (n + sl[I].Length+lineEndSize) > offset then
      begin
        Result.Line := I;
        Result.Char := Offset - n;
        Exit;
      end;
      n := n + sl[I].Length+lineEndSize;
    end;
  finally
    sl.Free;
  end;
end;


procedure TfrmMain.BCEditor1SelectionChanged(Sender: TObject);
var c,t:string;
begin
  c := BCEditor1.SelectedText;
  pas := c_to_pas(c,t);
  UpdateTree;

  BCEditor2.Text := pas.toPascal;
end;

procedure TfrmMain.GetRangeSource(const text: string; e: TPascalElement; out p1,p2: TBCEditorTextPosition);
begin
  p1 := getPosition(Text, e.Sourceinfo.Position);
  p2 := getPosition(Text, e.Sourceinfo.Position + e.Sourceinfo.Length);
end;

procedure TfrmMain.GetRangeRender(const text: string; e: TPascalElement; out p1,p2: TBCEditorTextPosition);
begin
  p1 := getPosition(Text, e.Renderinfo.Position);
  p2 := getPosition(Text, e.Renderinfo.Position + e.Renderinfo.Length);
end;


procedure TfrmMain.BCEditor2CaretChanged(ASender: TObject; X, Y: Integer);
var c:TClassdef;e:TPascalElement;
  p1,p2:tbceditortextposition;
  text:string;
begin
  StatusBar1.Panels[4].Text := Format('[Line:%d,Col%d]', [Y,X]);
  if pas=nil then
    Exit;

  text := BCEDitor2.Text;
  for c in pas.Classes do
    for e in c.Methods do
    begin
      GetRangeRender(text, e, p1, p2);
      if InRange(Y,p1.Line, p2.Line) then
      begin
        ShowElement(ASender,e);
        BCEditor1.Refresh;
        Exit;
      end;
    end;

  for e in pas.GlobalArrays1D do
  begin
    GetRangeRender(text, e, p1, p2);
    if InRange(Y,p1.Line, p2.Line) then
    begin
      ShowElement(ASender,e);
      BCEditor1.Refresh;
      Exit;
    end;
  end;

  for e in pas.GlobalArrays2D do
  begin
    GetRangeRender(text, e, p1, p2);
    if InRange(Y,p1.Line, p2.Line) then
    begin
      ShowElement(ASender,e);
      BCEditor1.Refresh;
      Exit;
    end;
  end;

end;

procedure TfrmMain.BCEditor2Change(Sender: TObject);
{$IFDEF USE_DELPHIAST}
var fn,t:string;
{$ENDIF}
begin
{$IFDEF USE_DELPHIAST}
  fn := TPath.Combine(TPath.GetTempPath, Pas.Name+'_tmp.pas');
  BCEditor2.Lines.SaveToFile(fn);
  t := Parse(fn,ex);
  ListBox1.Clear;
  ListBox1.Items.Add(t);
  TFile.Delete(fn);
{$ENDIF}
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
{$IFDEF USE_DWS}
  dws:=TDelphiWebScript.Create(nil);
{$ENDIF}
  CFileName := '';
  BCEditor1.Highlighter.LoadFromFile('Highlighters\c++.json');
  BCEditor1.Highlighter.Colors.LoadFromFile('Colors\Monokai.json');
  BCEditor2.Highlighter.LoadFromFile('Highlighters\object pascal.json');
  BCEditor2.Highlighter.Colors.LoadFromFile('Colors\Monokai.json');
//  BCEditor2.Highlighter.LoadFromFile('c:\dev\lib\Bone\TBCEditor\Highlighters\object pascal.json');
  DragAcceptFiles( Handle, True ) ;
  BCEditor1Change(nil);
end;

procedure TfrmMain.ListBox1DblClick(Sender: TObject);
var line,Col:integer;m:TMatch; pos:TBCEditorTextPosition;

begin
  if ListBox1.ItemIndex<0 then Exit;


  m := TRegEx.Match(ListBox1.Items[ListBox1.ItemIndex], '\[(?<Line>\d+)\,\s*(?<Col>\d+)\]\s*(?<Message>.*)');
  if not m.Success then
    Exit;

  Line := StrToInt(m.Groups['Line'].Value)-1;
  Col := StrToInt(m.Groups['Col'].Value);
//  Msg := m.Groups['Message'].Value;

  pos.Char := Col;
  pos.Line := Line;

  BCEditor2.SelectionBeginPosition := Pos;

  Pos.Char := Pos.Char + 1;
  BCEditor2.SelectionEndPosition := Pos;
  BCEditor2.CaretInView;

  BCEditor2.TopLine := Line - 3;
  BCEditor2.SetFocus;
  FocusControl(BCEditor2);


end;

{$IFDEF USE_DWS}
procedure TfrmMain.Run;
var s,code:string;
begin
  code := BCEditor2.Text;

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
      Result := TFile.ReadAllText(hfn) + sLineBreak + Result;;
  end;
end;

procedure TfrmMain.ShowElement(Sender:TObject; rt: TPascalElement);
var
  p1: TBCEditorTextPosition;
  p2: TBCEditorTextPosition;
begin
  p1 := getPosition(BCEditor1.Text, rt.Sourceinfo.Position);
  p2 := getPosition(BCEditor1.Text, rt.Sourceinfo.Position + rt.Sourceinfo.Length);
  while BCEditor1.Lines.GetLineText(p1.Line).Trim = '' do
  begin
    p1.Line := p1.Line + 1;
    if p1.Line > bceditor1.Lines.Count then
      Break;
  end;
  if Sender<>BCEditor1 then
  begin
    BCEditor1.SelectionBeginPosition := p1;
    BCEditor1.SelectionEndPosition := p2;
    BCEditor1.TopLine := p1.Line;
  end;

  p1 := getPosition(BCEditor2.Text, rt.Renderinfo.Position);
  p2 := getPosition(BCEditor2.Text, rt.Renderinfo.Position + rt.Renderinfo.Length);
  if Sender<>BCEditor2 then
  begin
    BCEditor2.SelectionBeginPosition := p1;
    BCEditor2.SelectionEndPosition := p2;
    BCEditor2.TopLine := p1.Line;
  end;
end;



procedure TfrmMain.SearchBox1Change(Sender: TObject);
begin
  BCEditor1.SearchString := SearchBox1.Text;
  BCEditor1.Search.SearchText := SearchBox1.Text;
  BCEditor1.FindNext;
  Caption := Format('Matches:%d', [BCEditor1.SearchResultCount]);;
end;

procedure TfrmMain.TreeView1Change(Sender: TObject; Node: TTreeNode);
var rt:TPascalElement;
begin
  if Node.Data=nil then
    Exit;

//  if Node.Text.StartsWith('function') or
//     Node.Text.StartsWith('procedure') or
//     Node.Text.Contains('array1D') or
///     Node.Text.Contains('array2D') then
  begin
    rt := TPascalElement(Node.Data);
    if rt=nil then
      Exit;
    ShowElement(Sender,rt);
  end;
end;

procedure TfrmMain.WMDROPFILES(var msg: TWMDropFiles);
const
  MAXFILENAME = 255;
var
  cnt, fileCount: integer;
  cfn,t:string;
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
      p := c_to_pas(ReadCCodeFromFile(cfn),t,changefileext(ExtractFilename(cfn),''));
      TFile.WriteAllText( ChangeFileExt(fileName,'.pas'), p.toPascal);
    end;
  end;

  if fileCount>0 then
  begin
    BCEditor1.Text := ReadCCodeFromFile(cfn);
    BCEditor1.Hint := ChangeFileExt(ExtractFileName(cfn),'');
    CFileName := fileName;
    BCEditor1Change(nil);
  end;

  DragFinish(msg.Drop);
end;

procedure TfrmMain.UpdateTree;
var
  n,
  unitNode,
  classNode: TTreeNode;
  c:TClassDef;rt:TRoutine;
  a1:TArrayDef1D;
  a2:TArrayDef2D;
  e: TEnumDef;

begin
  with TreeView1.Items do
  begin
    Clear;
    unitNode := Add(nil, pas.Name+':Unit').Data;
    for e in pas.Enums do
    begin
      n := AddChild(unitNode, e.name+':Enum');
      n.Data := e;
    end;

    for a1 in pas.GlobalArrays1D do
    begin
      n := AddChild(unitNode, a1.name+':array[0..'+a1.rangeMax+'] of '+a1.itemType);
      n.Data := a1;
    end;
    for a2 in pas.GlobalArrays2D do
    begin
      n := AddChild(unitNode, a2.name+':array[0..'+a2.ranges[0].rangeMax+',0..'+a2.ranges[1].rangeMax+'] of '+a2.itemType);
      n.Data := a2;
    end;



    for c in Pas.Classes do
    begin
      classNode := addChild(unitNode, c.Name+':'+cClassKind[c.Kind]);
      for rt in c.Methods do
      begin
        n := AddChild(classNode, cRoutineType[rt.RoutineType]+' ' + rt.Name);

        n.Data := rt;
      end;
    end;
    TreeView1.FullExpand;
  end;
end;

procedure TfrmMain.Action1Execute(Sender: TObject);
begin
  BCEditor2.Lines.SaveToFile(ChangeFileExt(cFileName,'.pas'));
end;

procedure TfrmMain.actRunExecute(Sender: TObject);
begin
  Run;
end;

procedure TfrmMain.BCEditor1CaretChanged(ASender: TObject; X, Y: Integer);
var c:TClassdef;e:TPascalElement;
  p1,p2:tbceditortextposition;
  text:string;
begin
  if pas=nil then
    Exit;

  text := BCEditor1.Text;

  for c in pas.Classes do
    for e in c.Methods do
    begin
      GetRangeSource(text, e, p1, p2);
      if InRange(Y,p1.Line, p2.Line) then
      begin
        ShowElement(ASender,e);
        Exit;
      end;
    end;

  for e in pas.GlobalArrays1D do
  begin
    GetRangeSource(text, e, p1, p2);
    if InRange(Y,p1.Line, p2.Line) then
    begin
      ShowElement(ASender,e);
      Exit;
    end;
  end;

  for e in pas.GlobalArrays2D do
  begin
    GetRangeSource(text, e, p1, p2);
    if InRange(Y,p1.Line, p2.Line) then
    begin
      ShowElement(ASender,e);
      Exit;
    end;
  end;
end;

procedure TfrmMain.BCEditor1Change(Sender:TObjecT);
var
  tl: Integer;t:string;
  c:string;
begin
  tl := BCEditor2.TopLine;
  c := BCEditor1.Text;
  pas := c_to_pas(c,t,BCEditor1.Hint);

  Caption := Application.Title;
  if Pas.name <> '' then
    Caption := pas.Name + ' - ' + Caption;

  UpdateTree;

  BCEditor2.Text := pas.toPascal;
  BCEditor2.TopLine := tl;
  BCEditor2.OnChange(sender);


end;

end.
