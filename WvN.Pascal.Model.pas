unit WvN.Pascal.Model;

interface

uses Generics.Collections, Generics.Defaults, Classes, Windows;

const
Keywords:array[0..65] of string=
('at','and','array','as','asm','begin','case','class','const','constructor',
 'destructor','dispinterface','div','do','downto','else','end','except','exports',
 'file','finalization','finally','for','function','goto','if','implementation',
 'in','inherited','initialization','inline','interface','is','label','library',
 'mod','nil','not','object','of','or','out','packed','procedure','program',
 'property','raise','record','repeat','resourcestring','set','shl','shr',
 'string','then','threadvar','to','try','type','unit','until','uses','var',
 'while','with','xor');

type
  TLanguage    = (Delphi,DWS);
  TDir         = (none,&in,&out,inout);
  TVisibility  = (&DefaultVisibility,&StrictPrivate,&Private,&Public,&Published);
  TRoutineType = (&function,&procedure,&constructor,&destructor);
  TClassKind   = (&class,&record,&object,&unit);
const
  cClassKind  : array[TClassKind] of string = ('class','record','object','unit');
  cDirPascal  : array[TDir] of string=('','const','out','var');
  cVisibility : array[TVisibility] of string=('','strict privateconst','private','public','published');
  cRoutineType: array[TRoutineType] of string=('function','procedure','constructor','destructor');

type
  TPascalUnit=class;

  TSourceInfo=record
    Position:integer;
    Length:integer;
    constructor Create(aPosition:integer; aLength:integer);
  end;

  TPascalElement=class;
{
  IPascalElement=interface
    procedure AddChild(const el:IPascalElement);
    function GetChildren(Index: integer): TPascalElement;
    function ChildIndexByName(const Name:string):integer;
    function ToString:string;
    function ToPascal:string;
    function Count:integer;
    procedure SetOwner(aOwner: IPascalElement);
    function GetName: string;
    property Children[Index:integer]:TPascalElement read GetChildren;
    property Name:string read GetName ;
  end;
}
  TPascalElement=class
  private
    function GetName: string;
    procedure SetOwner(aOwner: TPascalElement);
  protected
    FChildren:TObjectList<TPascalElement>;
    FOwner:TPascalElement;
    FName : String;
    FVisible:Boolean;
    procedure AddChild(const el:TPascalElement);
    function GetChildren(Index: integer): TPascalElement;
  public
    Sourceinfo:TSourceInfo;
    Renderinfo:TSourceInfo;
    function ChildIndexByName(const Name:string):integer;
    function ToString:string;reintroduce;virtual;
    function ToPascal:string;virtual;abstract;
    function Count:integer;
    procedure SetDefaultVisible;
    property Owner:TPascalElement read FOwner;
    constructor Create(aOwner:TPascalElement);virtual;
    destructor Destroy; override;
    property Children[Index:integer]:TPascalElement read GetChildren; default;
    property Name:string read GetName write FName;
    property Visible:Boolean read FVisible write FVisible;
  end;

  TVariable=class(TPascalElement)
  strict private
    FHasValue:Boolean;
    FDir:TDir;
    FType:string;
    FValue:variant;
    FVisibility:TVisibility;
    FIsStatic:Boolean;
    FComment:string;
  public
    constructor Create(AOwner:TPascalElement; aName:string; aType:string;aDir:TDir=TDir.none; aIsStatic:Boolean=false; aHasValue:Boolean=false; aValue:string=''; aComment:string=''); reintroduce;
    function ToString:string;override;
    function ToPascal:String;override;

    property HasValue:Boolean       read FHasValue;
    property Dir:TDir               read FDir;
    property &Type:string           read FType;
    property Value:variant          read FValue;
    property Visibility:TVisibility read FVisibility write FVisibility;
    property IsStatic:Boolean       read FIsStatic;
    property Comment:string         read FComment;

  end;

  TVariableList=class(TPascalElement)
  public
    function getLongestName:integer;
    function ToPascal(Indent:Boolean):String;reintroduce;
  end;

  TArrayDef1D=class(TPascalElement)
  public
    itemType:string;
    rangeMin,rangeMax:string;
    Items:TArray<string>;
    function ToPascal:string;override;
    function ToString:string;override;
  end;

  TArrayDef2D=class(TPascalElement)
  public
    itemType:string;
    ranges:array[0..1] of
    record
      rangeMin,rangeMax:string;
    end;
    Items:TArray<TArray<string>>;
    function ToPascal:string;override;
    function ToString:string;override;
  end;

  TEnumItem = record
    Index, Value:integer;
    Name:string;
    Comment:string;
    MaxNameLen,MaxValueLen:integer;
    class operator implicit(const e:TEnumItem):string;
  end;

  TEnumDef=class(TPascalElement)
  public
    Items:TArray<TEnumItem>;
    function ToPascal:string;override;
  end;

  TCode=class(TPascalElement)
  private
    function GetLineCount: integer;
  public
    Lines:TList<string>;
    procedure Add(const s:String);
    function ToPascal:String;override;
    procedure Cleanup;
    procedure Align;
    destructor Destroy; override;
    property LineCount:integer read GetLineCount;
    constructor Create(aOwner:TPascalElement; c:TArray<string>);reintroduce;

  end;

  TRoutine=class(TPascalElement)
  public
    RoutineType:TRoutineType;
    ReturnType:string;
    Parameters:TVariableList;
    LocalVars:TVariableList;
    Code:TCode;
    &Override:Boolean;
    &Overload:Boolean;
    &Inline:Boolean;
    &Static:Boolean;
    &Virtual:Boolean;
    ClassName:string;
    Comment:string;
    Visibility:TVisibility;

    constructor Create(
        aOwner:TPascalElement;
        aName:String;
        aClassName:string;
        aRoutineType:TRoutineType;
        aReturnType:string;
        aParameters:TVariableList;
        aLocalVars:TVariableList;
        aCode:TCode;
        aOverride:Boolean=false;
        aOverload:Boolean=false;
        aInline:Boolean=false;
        aStatic:Boolean=false;
        aVirtual:Boolean=false;
        aComment:string='');reintroduce;
    procedure Cleanup;
    function ToString:string;override;
    function ToDeclarationPascal:String;
    function ToImplementationPascal(aClassName:string):String;
    function Equals(Obj: TObject): Boolean; override;

  end;

  TClassDef=class(TPascalElement)
  public
    FKind:TClassKind;
    FParentType:string;
    FConsts:TVariableList;
    FMembers:TVariableList;
    FMethods:TArray<TRoutine>;
    FIsPacked:boolean;
    function AddRoutine(const m:TRoutine):boolean;
    function getMethodByName(const n:string):TRoutine;
    function ToPascalDeclaration:string;
    function ToPascalImplementation:string;
    function ToString:string;override;
    constructor Create(aOwner:TPascalElement;aTypename:string; aMembers:TVariableList; aKind:TClassKind);reintroduce;
  end;

  TUsesListItem=class(TPascalElement)

  end;

  TUsesList=class(TPascalElement)
  public
    &Unit:TPascalUnit;
    constructor Create(aOwner:TPascalElement);override;
    procedure AddUnit(const s: string);
    function ToPascal:string;override;
  end;

  TCase=class(TPascalElement)
  strict private
    FCode:TCode;
    procedure SetCode(const Value: TCode);
  public
    Id:string;
    function ToPascal(aIndent:integer=6; aAlign:integer=0):string;reintroduce;
    property Code:TCode read FCode write SetCode;

  end;

  TSwitch=class(TPascalElement)
  public
    Switch:string;
    Cases:TArray<TCase>;
    Indent:integer;
    function ToPascal:string;override;
  end;

  TIfStatement=class(TPascalElement)
  private
    Condition,
    IfTrue,
    IfFalse : TCode;
  public
    constructor Create(aOwner:TPascalElement; aCondition, aIfTrue, aIfFalse : TCode);reintroduce;
    function ToPascal:string;override;
  end;

  TPascalUnit=class(TPascalElement)
  public
    usesListIntf:TUsesList;
    usesListImpl:TUsesList;
    Comments:TArray<string>;
    GlobalVars:TVariableList;
    GlobalArrays1D:TArray<TArrayDef1D>;
    GlobalArrays2D:TArray<TArrayDef2D>;
    Enums:TArray<TEnumDef>;
    Classes:TArray<TClassDef>;
    ImplementationVars:TVariableList;
    &Initialization:TCode;
    &Finalization:TCode;
    CaseStatements:TArray<TSwitch>;
    Defines:TStringList;
    constructor Create(aOwner:TPascalElement);override;
    destructor Destroy;override;
    function getClassByName(s:string):TClassDef;
    function AddClass(c:TClassDef):TClassDef;
    function ToString:string; override;
    function ToPascal:string;override;
  end;

  TLoopOperator=(EQ,LT,LT_EQ,GT,GT_EQ);
  TLoop=class(TPascalElement)
  type
    TDir=(up,down);
  var
    IndexerVar:TVariable;
    Op:TLoopOperator;
    Dir:TLoop.TDir;
    StartVal,EndVal:string;
    function ToPascal:string;override;
  end;

function Esc(Keyword:string):string;

implementation

uses System.RegularExpressions, SysUtils, Math;

function Esc(Keyword:string):string;
var
  I: Integer;
begin
  Keyword := Keyword.Replace('*','');

  for I := 0 to High(Keywords) do
    if SameText(Keywords[I],Keyword) then
      Exit('&'+Keyword);

  Exit(Keyword);
end;

function MakePascalCase(const aString:String):String;
var
  I:Integer;
const
  Chars=[' ','-','_','/','\','&','*','|','(',')','[',']','{','}','.',';',',','<','>'];
begin
  if aString='' then
    Exit;

  Result := UpCase(aString[1]);

  for I := 2 to Length(aString) do
    if CharInSet(aString[I-1], Chars) then
      Result := Result + UpCase(aString[I])
    else
      Result := Result + aString[I];

  for I := Length(Result) downto 1 do
    if CharInSet(Result[I],Chars) then
      Delete(Result,I,1);
end;



constructor TVariable.Create(AOwner:TPascalElement; aName, aType: string;aDir:TDir=TDir.none;aIsStatic:Boolean=false; aHasValue:Boolean=false;aValue:string='';aComment:string='');
begin
  inherited Create(AOwner);

  FName     := aName.Trim;
  FType    := aType.Trim;
  FDir      := aDir;
  FIsStatic := aIsStatic;
  FHasValue := aHasValue;
  FValue    := aValue.Trim;
  FComment  := aComment;


  if FHasValue then
  if FType<>'' then
  if FDir = TDir.&in then
  begin
    // found a const variable declaration, with a type.
    if FType = 'integer'  then FType := '' else
    if FType = 'double'   then FType := '' else
    if FType = 'float'    then FType := '' else
    if FType = 'single'   then FType := '' else
    if FType = 'byte'     then FType := '' else
    if FType = 'int64'    then FType := '' else
    if FType = 'cardinal' then FType := '' else
      FDir := TDir.inout;
  end;
end;

function TVariable.ToPascal: String;
begin
  Result := Esc(FName);
  if FType<>''   then Result := Result + ':' + Esc(FType);
  if FHasValue    then Result := Result + ' = '+ FValue;
  if FComment<>'' then Result := Result + '{ '+FComment+' }';
end;

function TVariable.ToString: string;
begin
  REsult := FName + ': ' + FType;
end;

{ TVariableList }

function TVariableList.getLongestName: integer;
var
  I,l: Integer;
begin
  Result := 0;
  for I := 0 to Count-1 do
  begin
    l := length(esc(FChildren[I].Name));
    if l > Result then
      Result := l;
  end;

end;

function TVariableList.ToPascal(indent:Boolean): String;
var longest,i:integer;
  align:boolean;
  Prev,Curr,Next:TVariable;
begin
  Result := '';
  longest := getLongestName;

  align := true;
  // don't align single row list (as used in function parameters)
  if not indent then
    align := false;

  // don't align if we only have really short names
  if getLongestName < 8 then
    Align := false;

  // if we have a single item, don't bother aligning..
  if self.Count<2 then
    Align := false;

  Prev := nil;
  Next := nil;
  // combine consecutive parameters of the same type wherever possible
  for i := 0 to Count - 1 do
  begin
    if I > 0 then
      Prev := TVariable( FChildren[i - 1] );
    Curr := TVariable( FChildren[i    ] );
    if I < Count-1 then
      Next := TVariable( FChildren[i + 1] );


    if Indent then
      if (I=0)
      or (Prev.Visibility <>
          Curr.Visibility) then
      begin
        if Curr.Visibility<>TVisibility.DefaultVisibility then
          Result := Result + cVisibility[Curr.Visibility] + sLineBreak+ '  ';
      end;

    // for the first in a series, write the direction if given
    if (I = 0)
    or (Prev.Dir   <> Curr.Dir)
    or (Prev.&Type <> Curr.&Type) then
    begin
      if Indent then
        if I = 0 then
          if Curr.Dir = TDir.none then
            if Result.Trim = '' then
              Result := cDirPascal[TDir.inout] + sLineBreak + '  '
            else
              Result := Result + cDirPascal[TDir.inout]  + sLineBreak + '  ';

      if (I=0) or (Prev.Dir   <> Curr.Dir) then
        if (I=0) then
          Result := cDirPascal[Curr.Dir]
        else
          if Indent then
            // switched from list of consts to vars
            Result := Result.Trim + sLineBreak +sLineBreak +'' + cDirPascal[Curr.Dir]
          else
            Result := Result.Trim + cDirPascal[Curr.Dir];


      if not Indent then
        Result := Result + ' '
      else
        if cDirPascal[Curr.Dir] <> '' then
          Result := Result.TrimRight + sLineBreak + '  ';
    end;

    // if the next argument is of another type or direction, write the type
    if i < Count - 1 then
      if (Next.dir = Curr.dir) then
        if (Next.&Type = Curr.&Type) then
        if not Curr.HasValue then
        begin
          Result := Result + Esc(Curr.FName.Trim) + ', ';
          if align then
            Result := Result + sLineBreak+'  ';
          continue;
        end;


    if Align then
      Result := Result +copy(Esc(Curr.FName)+ StringOfChar(' ',longest) ,1,longest)
    else
      Result := Result + Esc(Curr.FName);

    if Curr.&Type<>'' then
    begin
      Result := Result + ' : ';

      if Curr.&Type='^' then
        Result := Result + 'pointer'
      else
      begin
        if Curr.FName.StartsWith('*') then
          Result := Result + '^';

        Result := Result + Esc(Curr.&Type);
      end;
    end;

    if Curr.HasValue then
      Result := Result + ' = '+ Curr.Value;

    if Curr.Comment<>'' then
      Result :=  Result + ' { '+ Curr.Comment + ' } ';

    // add a separator, unless it's the last argument
    if i < Count - 1 then
    begin
      Result := Result + ';';
      if Align then
        Result := REsult+sLineBreak+'  ';

    end;
  end;

  Result := Result.Replace(',   ',', ');

  if Indent then
  begin
    if not Result.Trim.EndsWith(';') then
      Result := Result.Trim + ';';

    Result := Result +sLineBreak;
  end;

end;

{ TRoutine }

procedure TRoutine.Cleanup;
begin
  if Assigned(Code) then
    Code.Cleanup;
end;

constructor TRoutine.Create(
        aOwner:TPascalElement;
        aName:String;
        aClassName:string;
        aRoutineType:TRoutineType;
        aReturnType:string;
        aParameters:TVariableList;
        aLocalVars:TVariableList;
        aCode:TCode;
        aOverride:Boolean=false;
        aOverload:Boolean=false;
        aInline:Boolean=false;
        aStatic:Boolean=false;
        aVirtual:Boolean=false;
        aComment:string='');
begin
  inherited Create(aOwner);

  Sourceinfo  := default(TSourceInfo);
  FName        := aName        ;
  ClassName   := aclassName   ;
  RoutineType := aRoutineType ;
  ReturnType  := aReturnType  ;
  if ReturnType='' then
    if RoutineType = TRoutineType.&function then
      RoutineType := TRoutineType.&procedure;
  if ReturnType='void' then
    RoutineType := TRoutineType.&procedure;

  Parameters  := aParameters  ;
  aParameters.SetOwner(self);
  LocalVars   := aLocalVars   ;
  LocalVars.SetOwner(self);
  Code        := aCode        ;
  Code.SetOwner(self);
  &Override   := aOverride    ;
  &Overload   := aOverload    ;
  &Inline     := aInline      ;
  &Static     := aStatic      ;
  &Virtual    := aVirtual     ;
  Comment     := aComment     ;
  Visibility  := TVisibility.DefaultVisibility;
end;

function TRoutine.Equals(Obj: TObject): Boolean;
var r:TRoutine; i:Integer;
begin
  if not (Obj is TRoutine) then
    Exit(False);

  if Obj=self then
    Exit(True);

  r := TRoutine(Obj);
  if not SameText(r.Name,Self.Name) then
    Exit(False);

  if r.Parameters.Count<>Self.Parameters.Count then
    Exit(False);

  for I := 0 to self.Parameters.Count-1 do
  begin
    if Parameters[I].Name <> r.Parameters[I].Name then
      Exit(False);
    if TVariable(Parameters[I]).&Type <> TVariable(r.Parameters[I]).&Type then
      Exit(False);
  end;
  Exit(True);

end;

function TRoutine.ToDeclarationPascal: String;
var sl:TStringBuilder; s:string;
begin
  sl := TStringBuilder.Create;
  try
    if Comment.Trim<>'' then
    begin
      sl.AppendLine('  /// <summary>');
      for s in Comment.Split([sLineBreak]) do
        sl.AppendLine('  ///   '+s);
      sl.AppendLine('  /// </summary>');
    end;

    sl.Append('  ');
    if &Static then
      if (FOwner=nil) or (TClassDef(FOwner).FKind <> TClassKind.&unit) then
        sl.Append('class ');
    sl.Append(cRoutineType[RoutineType]);
    sl.Append(' ');
    sl.Append(Esc(self.FName));
    if Parameters.Count > 0 then
      sl.Append( '(' + Parameters.ToPascal(false)+ ')' );

    if Self.RoutineType=TRoutineType.&function then
      sl.Append(':'+Self.ReturnType);

    if sl.ToString.Trim<>';' then
      sl.Append(';');

    if &Override then sl.Append('override;');
    if &Overload then sl.Append('overload;');
    if &Inline   then sl.Append('inline;');
    if &Static   then
      if (FOwner=nil) or (TClassDef(FOwner).FKind <> TClassKind.&unit) then
        sl.Append('static;');
    if &Virtual  then sl.Append('virtual;');

    Result := sl.ToString;
  finally
    sl.Free;
  end;
end;


function TRoutine.ToImplementationPascal(aClassName: string): String;
var sl:TStringBuilder; s:string;
// c: string;
begin
  sl := TStringBuilder.Create;
  try

    if Comment.Trim<>'' then
    begin
      sl.AppendLine('/// <summary>');
      for s in Comment.Split([sLineBreak]) do
        sl.AppendLine('///   '+s);
      sl.AppendLine('/// </summary>');
    end;

    sl.Append(cRoutineType[RoutineType]);
    sl.Append(' ');

    if aClassName<>'' then
    if Esc(aClassName)<>'TGlobal' then
      sl.Append(Esc(aClassName) + '.');
    sl.Append(Esc(self.FName));
    if Parameters.Count > 0 then
      sl.Append( '(' + Parameters.ToPascal(false) + ')' );

    if Self.RoutineType=TRoutineType.&function then
      sl.Append(':'+Self.ReturnType);

    sl.AppendLine(';');
    if self.LocalVars.Count>0 then
      sl.Append( LocalVars.ToPascal(true) );

    sl.AppendLine( 'begin');

    Code.Cleanup;
    Code.Align;

    code.Renderinfo.Position := sl.Length+1;
    sl.AppendLine(code.ToPascal);
    code.Renderinfo.Length   := sl.Length - code.Renderinfo.Position ;

    sl.AppendLine( 'end;');
    Result := sl.ToString;
  finally
    sl.Free;
  end;
end;

function TRoutine.ToString: string;
begin
  Result := FName+'( )';
end;

{ TClassDef }

function TClassDef.AddRoutine(const m: TRoutine):Boolean;
var i:integer;
begin
  Result := True;
  for I := 0 to high(FMethods) do
  begin
    if SameText(FMethods[I].Name ,m.Name) then
    begin
      FMethods[I].Overload := True;
      m.Overload := True;
    end;

    if FMethods[I].Equals(m) then
    begin
      Result := False;
      Break;
    end;
  end;

  if Result then
    FMethods := FMethods + [m]
  else
    if FMethods[I].Visibility = DefaultVisibility then
      FMethods[I].Visibility := m.Visibility;
end;


constructor TClassDef.Create(aOwner:TPascalElement; aTypename: string; aMembers: TVariableList;aKind:TClassKind);
begin
  inherited Create(aOwner);

  FKind := aKind;
  FIsPacked := false;

  FConsts := TVariableList.Create(self);
  FConsts.Name := 'Consts';

  if aMembers=nil then
    FMembers := TVariableList.Create(self)
  else
    FMembers := aMembers;

  FMembers.Name := 'Members';
  FMembers.FOwner := self;

  FName := aTypename;
  FIsPacked := false;

end;

function TClassDef.getMethodByName(const n: string): TRoutine;
var
  r: TRoutine;
begin
  Result := nil;
  for r in self.FMethods do
    if SameText(r.FName,n) then
      Exit(r);
end;

function TClassDef.ToPascalDeclaration: string;
var sl:TStringList;
  m: TRoutine;
begin
  sl := TStringList.Create;
  try

    if FKind <> TClassKind.&unit then
      sl.Add('type');

    case FKind of
      &class  :
        if FParentType='' then
          sl.Add(Esc(FName) + ' = class')
        else
          sl.Add(Esc(FName) + ' = class('+FParentType.Trim +')');
      &record :
        begin
          if FIsPacked then
            sl.Add(Esc(FName) + ' = packed record')
          else
            sl.Add(Esc(FName) + ' = record');
        end;
      &object : sl.Add(Esc(FName) + ' = object('+FParentType.Trim+')');
      &unit   : ;
    end;


    if FConsts.Count>0 then
    begin
      sl.Add('const');
      sl.Add( FConsts.ToPascal(true)  );
    end;

    if FMembers.Count>0 then
    begin
      if FConsts.Count>0 then
      begin
        sl.Add('');
        sl.Add('var');
      end;
      sl.Add( FMembers.ToPascal(true).TrimRight  );
    end;

    for m in FMethods  do
      sl.Add(m.ToDeclarationPascal);


    if self.FKind <> &unit then
      sl.Add( 'end;');

    result := sl.Text;
  finally
    sl.Free;
  end;
end;

function TClassDef.ToPascalImplementation: string;
var m:TRoutine; sl:TStringList;
begin
  sl := TStringList.Create;
  try
    if length(FMethods)=0 then
      Exit('');

    if self.FKind<>&unit then
    begin
      sl.Add( format('{ %s }',[self.FName]) );
      sl.Add('' );
    end;

    for m in FMethods  do
    begin
      m.Renderinfo.Position := sl.Text.Length+1;
      sl.Add(m.ToImplementationPascal( FName ));
      m.Code.Renderinfo.Position := m.Code.Renderinfo.Position + m.Renderinfo.Position;
      m.Renderinfo.Length   := sl.Text.Length - m.Renderinfo.Position-1;
      sl.Add('');
    end;

    Result := sl.Text;
  finally
    sl.free;
  end;

end;




function TClassDef.ToString: string;
begin
  if Self.FKind=&unit then
    Result := 'Global'
  else
    Result := cClassKind[ self.FKind ] + ' ' + FName;
end;

{ TCode }


procedure TCode.Add(const s: String);
begin
  Lines.Add(s);
end;

procedure DoAlign(Lines:TList<string>; var p:integer; aFirstLine,aLastLine,MaxPos:integer);
var J,t: Integer; s,oldLine:string;
begin
  if MaxPos>0 then
  begin
    for J := aFirstLine to aLastLine do
    begin
      oldLine := Lines[J];
      t := Pos(':=',OldLine);
      s := StringOfChar(' ', MaxPos-t);
      if P>3 then
        Insert(s,OldLine,t);
      Lines[J] := OldLine;
    end;
  end;
end;

procedure TCode.Align;
var
  Line:string; i,First,Last:integer;
  p,maxPos:integer;
begin
  First := -1;
  Last := -1;
  MaxPos := -1;
  for I := 0 to Lines.Count-1 do
  begin
    Line := Lines[I];
    P := Pos(':=',Line);
    if (P>0) and (not Line.Contains('for')) then
    begin
      if maxPos<0 then
        First := i;
      MaxPos := Max(maxPos,P);
      Last := i;
    end
    else
    begin
      DoAlign(Lines,P,First,last,maxPos);
      P := -1;
      First := -1;
      MaxPos := -1;
    end;
  end;
  if MaxPos>0 then
    DoAlign(Lines,p,First,last,maxPos);
end;

procedure TCode.Cleanup;
var i,j:integer;
  line:string;
begin
  // clean code
  j := 0;
  for I := 0 to Lines.Count-1 do
  begin
    line := Lines[I];
    if line='' then
      Continue;

    line := line.Replace(#9,'  ');
    if line.Trim<>'' then
    begin
      if lines.Count>1 then
      begin
        if I=0 then
          if line.Trim='begin' then
            Continue;

        if I=lines.Count-1 then
          if line.StartsWith('end') then
            Continue;
      end;

      lines[J] := line;
      inc(J);
    end;
  end;
  for I := Lines.Count-1 downto J do
    Lines.Delete(I);
end;

procedure TPascalElement.SetDefaultVisible;
  procedure SetVisible(const el:TPascalElement);
  var i:integer;
  begin
    if el = nil then
      Exit;

    if el is TVariableList then
      if el.Count=0 then
        el.Visible := False;

    if el is TCode then
      if el.Count=0 then
        el.Visible := False;

    if el is TUsesList then
      if el.Count=0 then
        el.Visible := False;

    if el.Owner<>nil then
      if not el.Owner.Visible then
        el.Visible := False;

    for I := 0 to el.Count-1 do
      SetVisible(el.FChildren[i]);
  end;

begin
  SetVisible(self);
end;

constructor TCode.Create(aOwner:TPascalElement;c: TArray<string>);
var i:integer;
begin
  inherited Create(aOwner);

  Lines := TList<string>.Create;
  Lines.InsertRange(0,c);
end;

destructor TCode.Destroy;
begin
  Lines.Free;
  inherited;
end;

function TCode.GetLineCount: integer;
begin
  Result := Lines.Count
end;

function TCode.ToPascal: String;
begin
  Result := string.join(sLineBreak, Lines.ToArray );
end;



{ TPascalUnit }

function TPascalUnit.AddClass(c: TClassDef):TClassDef;
begin
  Classes := Classes + [c];
  Result := c;
end;

constructor TPascalUnit.Create(aOwner:TPascalElement);
begin
  inherited Create(aOwner);

  Defines := TStringList.Create;

  usesListIntf := TUsesList.Create(self);
  usesListImpl := TUsesList.Create(self);

  &Initialization := TCode.Create(self,[]);
  &Initialization.FName := 'Initialization';
  &Finalization   := TCode.Create(self,[]);
  &Finalization.FName := 'Finalization';

  GlobalVars := TVariableList.Create(self);
  GlobalVars.FName := 'Global vars';
  ImplementationVars := TVariableList.Create(self);
  ImplementationVars.FName := 'Impl vars';
end;



destructor TPascalUnit.Destroy;
begin
  Defines.Free;
  inherited;
end;

function TPascalUnit.getClassByName(s: string): TClassDef;
var I:Integer;
begin
  for I := 0 to high(self.Classes) do
    if SameText( Classes[i].FName, s) then
      Exit( classes[I] );

  if SameText(s,'TGlobal') then
    Result := TClassDef.Create(Self,s,nil,TClassKind.&unit)
  else
    if s.ToLower.EndsWith('rec') then
      Result := TClassDef.Create(Self,s,nil,TClassKind.&record)
    else
      Result := TClassDef.Create(Self,s,nil,TClassKind.&class);

  Self.AddClass(Result);
end;

function TPascalUnit.toPascal: string;
var
  sl:TStringList;
  c:TClassDef;
  ar: TArrayDef1D;
  ar2: TArrayDef2D;
  e: TEnumDef;
  isProgram:Boolean;
  g:TClassDef;
  o:Integer;
  m:TRoutine;
begin
  sl := TStringList.Create;

  g := Self.getClassByName('TGlobal');

  // if the code contains a 'main' function, we probably want to generate
  // a program instead of a unit.
  isProgram := (g <> nil) and (g.getMethodByName('main') <> nil);

  // if no name is set, try to base it on a class name in the unit.
  if FName='' then
  begin
    FName := 'tmp';
    for c in classes do
      if c.FName<>'TGlobal' then
      begin
        FName := c.FName;
        if string(FName).StartsWith('T') then
          FName:= string(FName).TrimLeft(['T']);
        break;
      end;
  end;

  if isProgram then
    sl.Add('program '+MakePascalCase(FName)+';')
  else
    sl.Add('unit '+MakePascalCase(FName)+';');

  sl.Add('');

  if not isProgram then
  begin
    sl.Add('interface');
    sl.Add('');
  end;

  sl.Add(usesListIntf.ToPascal);

  for e in enums do
    sl.Add( e.ToPascal );

  for c in classes do
    if ((not isProgram) or (g.FKind<>&unit)) or (c.FKind=TClassKind.&record) then
    begin
      c.Renderinfo.Position := length(sl.Text)+1;
      sl.Add(c.ToPascalDeclaration);
      c.Renderinfo.Length   := length(sl.Text)-c.Renderinfo.Position;
    end;


 if GlobalVars.Count>0 then
   sl.Add(GlobalVars.ToPascal(true));


  if length(self.GlobalArrays1D)>0 then
  begin
    sl.Add('const');
    for ar in self.GlobalArrays1D do
    begin
      ar.Renderinfo.Position := sl.Text.Length+1;
      sl.Add(ar.ToPascal);
      ar.Renderinfo.Length   := sl.Text.Length - ar.Renderinfo.Position ;
    end;
  end;

  if length(self.GlobalArrays2D)>0 then
  begin
    sl.Add('const');
    for ar2 in self.GlobalArrays2D do
    begin
      ar2.Renderinfo.Position := sl.Text.Length+1;
      sl.Add(ar2.ToPascal);
      ar2.Renderinfo.Length   := sl.Text.Length - ar2.Renderinfo.Position ;
    end;
  end;

  if not isProgram then
  begin
    sl.Add('implementation');
    sl.Add('');
  end;

  sl.Add(usesListImpl.ToPascal);

  for c in classes do
  begin
    o := sl.Text.Length;
    sl.Add(c.ToPascalImplementation);
    for m in c.FMethods do
      m.Renderinfo.Position := m.Renderinfo.Position + o;
  end;
  sl.Add('');

  if not isProgram then
    if (Self.&Initialization.Count > 0) or (Self.&Finalization.Count > 0) then
    begin
      sl.Add('initialization');
      sl.Add(Self.&Initialization.ToPascal);
      sl.Add('');
      sl.Add('finalization');
      sl.Add(Self.&Finalization.ToPascal);
      sl.Add('');
    end;

  if isProgram then
  begin
    sl.Add('begin');
    sl.Add('  try');
    sl.Add('    Main;');
    sl.Add('  except');
    sl.Add('    on e:Exception do');
    sl.Add('      WriteLn(e.Message);');
    sl.Add('  end;');
  end;

  sl.Add('end.');
  result := sl.Text;
  sl.Free;
end;





function TPascalUnit.ToString: string;
begin
  if FName='TGlobal' then
    Exit('{global}');

  Result := 'Unit '+ FName
end;

{ TArrayDef }

function TArrayDef1D.ToPascal: string;
var elms : string; i:integer; it:TArray<string>;
begin
  setlength(it,length(items));
  for i := 0 to high(items) do
  begin
    it[i] := items[i];
    if it[i].EndsWith('.') then
      it[i] := it[i] + '0';
    if it[i].StartsWith('.') then
      it[i] := '0'+it[i];

  end;

  elms := ''.Join(', ', it); // concat all elements into a comma separated string
  elms := TRegEx.Replace(elms,'0[xX]([\da-fA-F]+)' ,'\$\1',[ roMultiLine ]); // convert possible hex to pascal hex
  elms := elms.Replace('/*','{').Replace('*/','}');
  elms := WrapText(elms,sLineBreak+'   ',[','],70); // wrap long lines

  Result :=
    format( '  %s : array[0..%d] of %s = ('+sLineBreak+'    %s );'+sLineBreak,[
      FName,
      length(Items)-1,
      itemType,
      elms
    ]);

end;

{ TLoop }

function TLoop.toPascal: string;
var v:string;t:integer;
const cLoopDir:array[TLoop.TDir] of string=('to','downto');
begin
  case Op of
    LT    : if TryStrToInt(EndVal, t) then
              v := IntToStr(t - 1)
            else
              v := EndVal + '-1';

    LT_EQ :   v := EndVal;

    GT    : if TryStrToInt(EndVal, t) then
              v := IntToStr(t + 1)
            else
              v := EndVal + '+1';

    GT_EQ :  v := EndVal;

    EQ    :  v := EndVal;
  end;

  Result := Format('for %s := %s %s %s do', [
              IndexerVar.FName,    // for XX
              StartVal,           // :=  XX
              cloopDir[self.Dir], // to/downto
              v                   //  XX
              ]);

end;


{ TUsesList }

constructor TUsesList.Create(aOwner:TPascalElement);
begin
  inherited Create(aOwner);
  self.&Unit := aOwner as TPascalUnit;
end;

procedure TUsesList.AddUnit(const s: string);
var t:string; el:TPascalElement;
begin
  if &unit<>nil then
    if SameText(s,&Unit.FName) then
      exit;

  for el in FChildren do
    if SameText(s,el.Name) then
      Exit;

  el := TUsesListItem.Create(self);
  el.Name := s;
end;

function TUsesList.ToPascal: string;
var units:TArray<string>; el:TPascalElement;
begin
  Result := '';
  Units := [];
  for el in FChildren do
    Units := Units + [el.Name];

  if Count>0 then
  begin
    Result := 'uses '+string.join(', ', Units)+';';
    Result := WrapText(Result, sLineBreak + '  ', [',',' ',#13,#10], 80 )+sLineBreak;
  end;
end;

{ TEnumDef }


function FormatInt(i:int64):string;
var h,d:string;
begin
  h := IntToHex(i,1);
  d := IntToStr(i);
  if h.TrimRight(['0']).Length <
     d.TrimRight(['0']).Length then
    Result := HexDisplayPrefix + h
  else
    Result := d;
end;


function TEnumDef.ToPascal: string;
var
  a:TArray<string>; nl,vl,i:integer;
begin
  nl := 0; vl := 0;
  for I := 0 to high(Items) do
  begin
    if Items[I].Name.Length > nl then
      nl := Items[I].Name.Length;

    if FormatInt( Items[I].Value ).Length > vl then
      vl := FormatInt( Items[I].Value ).length;
  end;

  for I := 0 to high(Items) do
  begin
    Items[I].MaxNameLen  := nl;
    Items[I].MaxValueLen := vl;
  end;

  // we could get an anonymous enum type
  // we'll convert it to a list of consts
  if FName='' then
  begin
    setlength(a,length(Items));
    for I := 0 to high(Items) do
      a[I] := Items[I];

    Exit('  '+''.Join(';'+sLineBreak+'  ',a )+sLineBreak);
  end;


  setlength(a,length(Items));
  for I := 0 to high(Items) do
    a[I] := Items[I];

  Result := '  '+ Esc(FName) +' = (' + sLineBreak +
            '    '+ ''.Join(','+sLineBreak+'    ',a ) +
            ');'+sLineBreak;
end;

{ TEnumItem }

class operator TEnumItem.implicit(const e: TEnumItem): string;
var v:string;
begin
  Result := copy(Esc(e.Name)+StringOfChar(' ',e.MaxNameLen),1,e.MaxNameLen);
//  if e.Index<>e.Value then
  begin
    v := FormatInt( e.Value );
    Result:= Result + ' = ' + StringOfChar(' ',e.MaxValueLen - v.Length) + v;
  end;

  if e.Comment <> ''  then
    Result := Result + ' { ' + e.Comment +' }';
end;

{ TSourceInfo }

constructor TSourceInfo.Create(aPosition, aLength: integer);
begin
  Position := aPosition;
  Length   := aLength  ;
end;

{ TArrayDef2D }

function TArrayDef2D.ToPascal: string;
var elms : string;a,b:TArray<string>;
begin
  b := [];
  for a in Items do
  begin
    elms := ''.Join(', ', a); // concat all elements into a comma separated string
    elms := TRegEx.Replace(elms,'0[xX]([\da-fA-F]+)' ,'\$\1',[ roMultiLine ]); // convert possible hex to pascal hex
    elms := elms.Replace('/*','{').Replace('*/','}');
    elms := WrapText(elms,sLineBreak+'   ',[','],200); // wrap long lines
    b := b + ['('+elms+')'];
  end;

  if length(items)>0 then
  Result :=
    format( '  %s : array[0..%d,0..%d] of %s = ('+sLineBreak+'    ',[
      FName,
      length(Items)-1,
      length(Items[0])-1,
      itemType
    ]);

  Result := Result + ''.join(','+sLineBreak+'    ',b);

  Result := Result + ');'+sLineBreak;
end;

procedure TCase.SetCode(const Value: TCode);
begin
  FCode := Value;
  FCode.SetOwner(self);
end;

function TCase.ToPascal(aIndent:integer=6; aAlign:integer=0):string;
var codelines:TArray<string>;
  I: Integer;
  indent:string;
const
  MaxAlign=15;
begin
  Indent := StringOfChar(' ',aindent);
  // remove break if it's the last statement:
  codeLines := code.Lines.ToArray;
  // remove break.. not needed in pascal
  if length(codelines)>0 then
    if codelines[high(codelines)].startswith('break') then
      setlength(codelines,length(codelines)-1);

  codeLines := string.Join(';',codelines).Trim.Split([sLineBreak]);


  aAlign := min(MaxAlign, aAlign);
  if Id.Length>MaxAlign then
    Result := Indent + '  '+self.Id
  else
    Result := Indent + '  '+copy(Id+ StringOfChar(' ',50) ,1, aAlign);

  if not SameText(id,'else') then
    Result := Result  + ':';

  Result := Result  + ' ';

  case Length(codelines) of
    0: Result := Result + ';' + sLineBreak;
    1: Result := Result + codelines[0] + ';' + sLineBreak;
  else
    begin
      Result := Result + sLineBreak +
                Indent+'    begin' + sLineBreak;

      for I := 0 to high(codelines) do
        Result := Result + Indent + '      '+codelines[I].Trim+ sLineBreak;

      Result := Result + Indent+'    end;' + sLineBreak;
    end;
  end;
end;

function TSwitch.ToPascal;
var
  i:integer;
  indentStr:string;
  align:integer;
begin
  align := 0;
  for I := 0 to high(Cases) do
    if cases[I].Id.Length > align then
      align := cases[I].Id.Length;

  indentStr:=StringOfChar(' ',indent);
  Result := indentStr+'case '+ Switch.Trim + ' of' + sLineBreak;
  for I := 0 to high(Cases) do
    Result := Result + cases[I].ToPascal(Indent, Align);
  Result := Result + sLineBreak + indentStr+ 'end; // case' + sLineBreak;

end;



function TArrayDef2D.ToString: string;
begin
  if Length(Items)<1 then
    Result :=
      format( '%s : array[0..%d] of %s',[
        FName,
        length(Items)-1,
        itemType
      ])
  else
    Result :=
      format( '%s : array[0..%d,0..%d] of %s',[
        FName,
        length(Items)-1,
        length(Items[0])-1,
        itemType
      ]);
end;

{ TPascalElement }

procedure TPascalElement.AddChild(const el: TPascalElement);
begin
  if not Assigned(el) then
    Exit;

  if not (el is TPascalElement) then
    Exit;

  TPascalElement(el).FOwner := self;
  FChildren.Add(el);
end;


function TPascalElement.ChildIndexByName(const Name: string): integer;
var I:Integer;
begin
  Result := -1;
  for I := 0 to Count-1 do
    if SameText(FChildren[I].FName,Name) then
      Exit(I);
end;

function TPascalElement.Count: integer;
begin
  Result := FChildren.Count;
end;

constructor TPascalElement.Create(aOwner: TPascalElement);
begin
  FVisible  := True;
  FChildren := TObjectList<TPascalElement>.Create;

  self.Sourceinfo := default(TSourceInfo);
  self.Renderinfo := default(TSourceInfo);

  SetOwner(aOwner);
end;


destructor TPascalElement.Destroy;
begin
  FreeAndNil(FChildren);
  inherited;
end;


function TPascalElement.GetChildren(Index: integer): TPascalElement;
begin
  Result := FChildren[Index]
end;

function TPascalElement.GetName: string;
begin
  Result := FName;
end;

procedure TPascalElement.SetOwner(aOwner: TPascalElement);
var I:integer;
begin
  if FOwner <> nil then
    if FOwner<>aOwner as TPascalElement then
      raise Exception.CreateFmt('Cannot reassign owner (%s) for %s. Owner already set to %s ',[ AOwner.ToString, Self.ToString, FOwner.ToString ] );

//  if AOwner = nil then raise Exception.Create('Owner cannot be set to nil');

  if aOwner = nil then
    Exit;

  for I := 0 to AOwner.Count-1 do
    if AOwner.FChildren[I] as TPascalElement = self then
      exit;
//      raise Exception.Create('Cannot add same element twice'+sLineBreak+aOwner.ToString);

  FOwner := aOwner as TPascalElement;
  FOwner.AddChild(self);
end;

function TPascalElement.ToString: string;
begin
  Result := ClassName.Substring(1) + ': ' + FName
end;


constructor TIfStatement.Create(aOwner:TPascalElement; aCondition, aIfTrue, aIfFalse: TCode);
begin
  inherited Create(aOwner);

  Condition := aCondition;
  aIfTrue := aIfTrue;
  aIfFalse := aIfFalse;

  Condition.SetOwner(self);
  aIfTrue.SetOwner(self);
  aIfFalse.SetOwner(self);
end;

function TIfStatement.ToPascal: string;
begin
  Result := '  if '+ Condition.ToPascal + ' then'+sLineBreak+ IfTrue.ToPascal;

  if IfFalse.Count>0 then
    Result := Result + ' else '+sLineBreak +  ifFalse.ToPascal;
end;

function TArrayDef1D.ToString: string;
begin
  Result :=
    format( '%s : array[0..%d] of %s',[
      FName,
      length(Items)-1,
      itemType
    ]);

end;

end.
