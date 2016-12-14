unit WvN.Pascal.Model;

interface

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
  TRoutineType = (&function,&procedure,&constructor);
  TClassKind   = (&class,&record,&object,&unit);
const
  cClassKind  : array[TClassKind] of string = ('class','record','object','unit');
  cDirPascal  : array[TDir] of string=('','const','out','var');
  cVisibility : array[TVisibility] of string=('','strict privateconst','private','public','published');
  cRoutineType: array[TRoutineType] of string=('function','procedure','constructor');

type
  TPascalUnit=class;

  TSourceInfo=record
    Position:integer;
    Length:integer;
    constructor Create(aPosition:integer; aLength:integer);
  end;

  TPascalElement=class abstract
    Name:String;
    Sourceinfo:TSourceInfo;
    Renderinfo:TSourceInfo;
  end;

  TVariable=class
  public
    HasValue:Boolean;
    Dir:TDir;
    Name:string;
    &Type:string;
    Value:variant;
    Visibility:TVisibility;
    IsStatic:Boolean;
    Comment:string;
    constructor Create;overload;
    constructor Create(aName:string; aType:string;aDir:TDir=TDir.none; aIsStatic:Boolean=false; aHasValue:Boolean=false; aValue:string=''; aComment:string='');overload;
    function ToPascal:String;
  end;

  TVariableList=record
  public
    Items:TArray<TVariable>;
    procedure Clear;
    procedure Add(v:TVariable);
    function getLongestName:integer;
    function ToPascal(Indent:Boolean):String;
    function Count:integer;
    class function CreateEmpty:TVariableList;static;
  end;

  TArrayDef1D=class(TPascalElement)
    itemType:string;
    rangeMin,rangeMax:string;
    Items:TArray<string>;
    function ToPascal:string;
  end;

  TArrayDef2D=class(TPascalElement)
    itemType:string;
    ranges:array[0..1] of
    record
      rangeMin,rangeMax:string;
    end;
    Items:TArray<TArray<string>>;

    function ToPascal:string;
  end;

  TEnumItem = record
    Index, Value:integer;
    Name:string;
    Comment:string;
    MaxNameLen,MaxValueLen:integer;
    class operator implicit(const e:TEnumItem):string;
  end;

  TEnumDef=class(TPascalElement)
    Items:TArray<TEnumItem>;
    function ToPascal:string;
  end;

  TCode=record
  private
    function GetCount: integer;
  public
    Lines:TArray<string>;
    constructor Create(c:TArray<string>);
    procedure Add(const s:String);
    function ToPascal:String;
    procedure Cleanup;
    procedure Align;
    property Count:integer read GetCount;
    class function CreateEmpty:TCode;static;
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
    ClassName:string;
    Comment:string;
    Visibility:TVisibility;

    constructor Create(
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
        aComment:string='');

    procedure Cleanup;
    function ToDeclarationPascal:String;
    function ToImplementationPascal(aClassName:string):String;
  end;

  TClassDef=class(TPascalElement)
    &Kind:TClassKind;
    ParentType:string;
    consts:TVariableList;
    Vars:TVariableList;
    Methods:TArray<TRoutine>;
    IsPacked:boolean;
    procedure Cleanup;
    procedure AddRoutine(const m:TRoutine);
    function getMethodByName(const n:string):TRoutine;
    function ToPascalDeclaration:string;
    function ToPascalImplementation:string;
    constructor Create;overload;
    constructor Create(aTypename:string; aVars:TVariableList);overload;
  end;

  TUsesList=record
  private
    function getCount:integer;
  public
    Units:TArray<string>;
    &Unit:TPascalUnit;
    constructor Create(aUnit:TPascalUnit);
    procedure AddUnit(s:string);
    function ToPascal:string;
    property Count:integer read getCount;
  end;

  TPascalUnit=class
    Name:String;
    usesListIntf:TUsesList;
    usesListImpl:TUsesList;
    Comments:TArray<string>;
    GlobalVars:TVariableList;
    GlobalArrays1D:TArray<TArrayDef1D>;
    GlobalArrays2D:TArray<TArrayDef2D>;
    ImplementationVars:TVariableList;
    Enums:TArray<TEnumDef>;
    Classes:TArray<TClassDef>;
    &Initialization:TCode;
    &Finalization:TCode;
    procedure Cleanup;
    function getClassByName(s:string):TClassDef;
    function AddClass(c:TClassDef):TClassDef;
    constructor Create;
    function toPascal:string;
  end;

  TLoop=record
    IndexerVar:TVariable;
    Op:(EQ,LT,LT_EQ,GT,GT_EQ);
    Dir:(up,down);
    StartVal,EndVal:string;
    function toPascal:string;
  end;

function Esc(Keyword:string):string;

implementation

uses System.RegularExpressions, SysUtils, Classes, Math;

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


constructor TVariable.Create;
begin

end;

constructor TVariable.Create(aName, aType: string;aDir:TDir=TDir.none;aIsStatic:Boolean=false; aHasValue:Boolean=false;aValue:string='';aComment:string='');
begin
  Name     := aName.Trim;
  &Type    := aType.Trim;
  Dir      := aDir;
  IsStatic := aIsStatic;
  HasValue := aHasValue;
  Value    := aValue.Trim;
  Comment  := aComment;
end;

function TVariable.ToPascal: String;
begin
  Result := Esc(Name);
  if &type<>'' then
    Result := Result + ':' + Esc(&Type);

  if HasValue then
    Result := Result + ' = '+ Value;

  if Comment<>'' then
    Result := Result + '{ '+Comment+' }';
end;

{ TVariableList }

procedure TVariableList.Add(v: TVariable);
begin
  setlength(Items,length(items)+1);


  if v.HasValue then
    if v.&Type<>'' then
      if v.Dir = TDir.&in then
  begin
    // found a const variable declaration, with a type.
    if v.&Type = 'integer'  then v.&Type := '' else
    if v.&Type = 'double'   then v.&Type := '' else
    if v.&Type = 'float'    then v.&Type := '' else
    if v.&Type = 'single'   then v.&Type := '' else
    if v.&Type = 'byte'     then v.&Type := '' else
    if v.&Type = 'int64'    then v.&Type := '' else
    if v.&Type = 'cardinal' then v.&Type := '' else
    v.Dir := TDir.inout;
  end;

  Items[high(items)] := v;
end;

procedure TVariableList.Clear;
begin
  Items := [];
end;

function TVariableList.Count: integer;
begin
  Result := Length(self.Items);
end;

class function TVariableList.CreateEmpty: TVariableList;
begin
  setlength(Result.Items,0);
end;

function TVariableList.getLongestName: integer;
var
  I,l: Integer;
begin
  Result := 0;
  for I := 0 to Count-1 do
  begin
    l := length(esc(Items[I].Name));
    if l > Result then
      Result := l;
  end;

end;

function TVariableList.ToPascal(indent:Boolean): String;
var longest,i:integer;
  align:boolean;
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

  // combine consecutive parameters of the same type wherever possible
  for i := 0 to Count - 1 do
  begin
    if Indent then
      if (I=0)
      or (Items[i - 1].Visibility <>
          Items[i    ].Visibility) then
      begin
        if Items[I].Visibility<>TVisibility.DefaultVisibility then
          Result := Result + cVisibility[Items[I].Visibility] + sLineBreak+ '  ';
      end;

    // for the first in a series, write the direction if given
    if (I = 0)
    or (Items[I - 1].Dir   <> Items[I].Dir)
    or (Items[I - 1].&Type <> Items[I].&Type) then
    begin
      if Indent then
        if I = 0 then
          if Items[I].Dir = TDir.none then
            if Result.Trim = '' then
              Result := cDirPascal[TDir.inout] + sLineBreak + '  '
            else
              Result := Result + cDirPascal[TDir.inout]  + sLineBreak + '  ';

      if (I=0) or (Items[I - 1].Dir   <> Items[I].Dir) then
        if (I=0) then
          Result := cDirPascal[Items[I].Dir]
        else
          if Indent then
            // switched from list of consts to vars
            Result := Result.Trim + sLineBreak +'' + sLineBreak +'' + cDirPascal[Items[I].Dir]
          else
            Result := Result.Trim + cDirPascal[Items[I].Dir];


      if not Indent then
        Result := Result + ' '
      else
        if cDirPascal[Items[I].Dir] <> '' then
          Result := Result + sLineBreak + '  ';
    end;

    // if the next argument is of another type or direction, write the type
    if i < Count - 1 then
      if (Items[i + 1].dir = Items[i].dir) then
        if (Items[i + 1].&Type = Items[i].&Type) then
        if not Items[I].HasValue then
        begin
          Result := Result + Esc(Items[i].name.Trim) + ', ';
          if align then
            Result := Result + sLineBreak+'  ';
          continue;
        end;


    if Align then
      Result := Result +copy(Esc(Items[i].name)+ StringOfChar(' ',longest) ,1,longest)
    else
      Result := Result + Esc(Items[i].name);

    if Items[i].&Type<>'' then
    begin
      Result := Result + ' : ';

      if Items[I].&Type='^' then
        Result := Result + 'pointer'
      else
      begin
        if Items[I].Name.StartsWith('*') then
          Result := Result + '^';

        Result := Result + Esc(Items[i].&Type);
      end;
    end;
    if Items[i].HasValue then
      Result := Result + ' = '+ Items[i].Value;

    if Items[i].Comment<>'' then
      Result :=  Result + ' { '+ Items[i].Comment + ' } ';

    // add a separator, unless it's the last argument
    if i < Count - 1 then
        Result := Result + ';';


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
  Code.Cleanup;
end;

constructor TRoutine.Create(
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
        aComment:string='');
begin
  Sourceinfo  := default(TSourceInfo);
  Name        := aName        ;
  ClassName   := aclassName   ;
  RoutineType := aRoutineType ;
  ReturnType  := aReturnType  ;
  if ReturnType='' then
    if RoutineType = TRoutineType.&function then
      RoutineType := TRoutineType.&procedure;
  if ReturnType='void' then
    RoutineType := TRoutineType.&procedure;

  Parameters  := aParameters  ;
  LocalVars   := aLocalVars   ;
  Code        := aCode        ;
  &Override   := aOverride    ;
  &Overload   := aOverload    ;
  &Inline     := aInline      ;
  &Static     := aStatic      ;
  Comment     := aComment     ;
  Visibility  := TVisibility.DefaultVisibility;
end;

function TRoutine.ToDeclarationPascal: String;
var sl:TStringBuilder; s:string;
begin
  sl := TStringBuilder.Create;
  for s in Comment.Split([sLineBreak]) do
    sl.AppendLine('// '+Comment);

  sl.Append('  ');
  if &Static then
    sl.Append('class ');
  sl.Append(cRoutineType[RoutineType]);
  sl.Append(' ');
  sl.Append(Esc(self.Name));
  if length(Parameters.Items)>0 then
    sl.Append( '(' + Parameters.ToPascal(false) + ')' );

  if Self.RoutineType=TRoutineType.&function then
    sl.Append(':'+Self.ReturnType);

  if sl.ToString.Trim<>';' then
    sl.Append(';');

  if &Override then sl.Append('override;');
  if &Overload then sl.Append('overload;');
  if &Inline   then sl.Append('inline;');
  if &Static   then sl.Append('static;');

  Result := sl.ToString;
  sl.Free;
end;


function TRoutine.ToImplementationPascal(aClassName: string): String;
var sl:TStringBuilder;
// c: string;
begin
  sl := TStringBuilder.Create;
  sl.Append(cRoutineType[RoutineType]);
  sl.Append(' ');

  if aClassName<>'' then
  if Esc(aClassName)<>'TGlobal' then
    sl.Append(Esc(aClassName) + '.');
  sl.Append(Esc(self.Name));
  if length(Parameters.Items)>0 then
    sl.Append( '(' + Parameters.ToPascal(false) + ')' );

  if Self.RoutineType=TRoutineType.&function then
    sl.Append(':'+Self.ReturnType);

  sl.AppendLine(';');
  if self.LocalVars.Count>0 then
    sl.Append( LocalVars.ToPascal(true) );

  sl.AppendLine( 'begin');
  Code.Cleanup;

  sl.AppendLine(code.ToPascal);

  sl.AppendLine( 'end;');
  Result := sl.ToString;
  sl.Free;
end;

{ TClassDef }

procedure TClassDef.AddRoutine(const m: TRoutine);
begin
  SetLength(Methods,length(Methods)+1);
  Methods[High(Methods)] := m;
end;

procedure TClassDef.Cleanup;
var i:integer;
begin
  for i := 0 to length(Methods)-1 do
    Methods[i].Cleanup;
end;

constructor TClassDef.Create;
begin
  self.Kind := TClassKind.&class;
  self.IsPacked := false;
  self.Vars := TVariableList.CreateEmpty;
end;

constructor TClassDef.Create(aTypename: string; aVars: TVariableList);
begin
  self.Name := aTypename;
  self.Vars := aVars;
  self.Kind := TClassKind.&class;
  self.IsPacked := false;

end;

function TClassDef.getMethodByName(const n: string): TRoutine;
var
  r: TRoutine;
begin
  Result := nil;
  for r in self.Methods do
    if SameText(r.Name,n) then
      Exit(r);
end;

function TClassDef.ToPascalDeclaration: string;
var sl:TStringList;
  m: TRoutine;
begin
  sl := TStringList.Create;

  if Kind <> TClassKind.&unit then
    sl.Add('type');

  case Kind of
    &class  :
      if ParentType='' then
        sl.Add(Esc(Name) + ' = class')
      else
        sl.Add(Esc(Name) + ' = class('+ParentType+')');
    &record :
      begin
        if IsPacked then
          sl.Add(Esc(Name) + ' = packed record')
        else
          sl.Add(Esc(Name) + ' = record');
      end;
    &object : sl.Add(Esc(Name) + ' = object('+ParentType+')');
    &unit   : ;
  end;


  if consts.Count>0 then
  begin
    sl.Add('const');
    sl.Add( consts.ToPascal(true)  );
  end;

  if vars.Count>0 then
  begin
    if consts.Count>0 then
    begin
      sl.Add('');
      sl.Add('var');
    end;
    sl.Add( vars.ToPascal(true).TrimRight  );
  end;

  for m in Methods  do
    sl.Add(m.ToDeclarationPascal);


  if self.Kind <> &unit then
    sl.Add( 'end;');

  result := sl.Text;
  sl.Free;
end;

function TClassDef.ToPascalImplementation: string;
var m:TRoutine; sl:TStringList;
begin
  sl := TStringList.Create;
  if length(Methods)=0 then
    Exit('');

  if self.Kind<>&unit then
  begin
    sl.Add( format('{ %s }',[self.Name]) );
    sl.Add('' );
  end;

  for m in Methods  do
  begin
    m.Renderinfo.Position := sl.Text.Length+1;
    sl.Add(m.ToImplementationPascal( name ));
    m.Renderinfo.Length   := sl.Text.Length - m.Renderinfo.Position ;
    sl.Add('');
  end;

  Result := sl.Text;
  sl.free;

end;




{ TCode }


procedure TCode.Add(const s: String);
begin
  SetLength(Lines,length(Lines)+1);
  Lines[High(Lines)] := s;
end;

procedure TCode.Align;
var I,J,f,p,mp:integer;

begin
  I := 0;
  mp := 0;
  while I<length(Lines) do
  begin
    p := lines[I].IndexOf(':=');
    if p>0 then
    begin
      f := I;
      while p>0 do
      begin
        if Lines[I].Trim.StartsWith('for') then
          p := 0;
        mp := max(p,mp);
        p := lines[I].IndexOf(':=');
        inc(I);
      end;

      if mp>5 then
      for J := f to I do
      begin
        lines[J] :=

          copy(copy(lines[J],1,lines[J].IndexOf(':='))+ StringOfChar(' ', 255) ,1,mp) +
          copy(lines[J],lines[J].IndexOf(':='),255);
      end;
      mp := 0;
    end;
    inc(i);
  end;
end;

procedure TCode.Cleanup;
var i,j:integer;
  line:string;
begin
  // clean code
  j := 0;
  for I := 0 to high(Lines) do
  begin
    line := Lines[I];
    if line='' then
      Continue;

    line := line.Replace(#9,'  ');
    if line.Trim<>'' then
    begin
      if length(lines)>1 then
      begin
        if I=0 then
          if line.Trim='begin' then
            Continue;

        if I=high(lines) then
          if line.StartsWith('end') then
            Continue;
      end;

      lines[J] := line;
      inc(J);
    end;
  end;
  Setlength(lines,J);
end;

constructor TCode.Create(c: TArray<string>);
var i:integer;
begin
  setlength(lines,length(c));
  for I := 0 to length(c)-1 do
    lines[I] := c[i];
end;

class function TCode.CreateEmpty: TCode;
begin
  //
end;

function TCode.GetCount: integer;
begin
  Result := Length(Lines)
end;

function TCode.ToPascal: String;
begin
  Result := ''.join(sLineBreak,self.Lines);
end;



{ TPascalUnit }

function TPascalUnit.AddClass(c: TClassDef):TClassDef;
begin
  SetLength(Self.Classes,length(self.Classes)+1);
  Classes[High(Classes)] := c;
  Result := c;
end;

procedure TPascalUnit.Cleanup;
var c:TClassDef;
begin
  &Initialization.Cleanup;
  &Finalization.Cleanup;
  for c in Classes do
    c.Cleanup;
end;

constructor TPascalUnit.Create;
begin
end;

function TPascalUnit.getClassByName(s: string): TClassDef;
var I:Integer;
begin
  for I := 0 to high(self.Classes) do
    if SameText( Classes[i].Name, s) then
      Exit( classes[I] );

  Result := nil;
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
  Cleanup;
  sl := TStringList.Create;

  g := Self.getClassByName('TGlobal');

  // if the code contains a 'main' function, we probably want to generate
  // a program instead of a unit.
  isProgram := (g <> nil) and (g.getMethodByName('main') <> nil);

  // if no name is set, try to base it on a class name in the unit.
  if Name='' then
  begin
    Name := 'tmp';
    for c in classes do
      if c.Name<>'TGlobal' then
      begin
        name := c.Name;
        if name.StartsWith('T') then
          name:= name.TrimLeft(['T']);
        break;
      end;
  end;

  if isProgram then
    sl.Add('program '+Name+';')
  else
    sl.Add('unit '+Name+';');

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
    if ((not isProgram) or (g.Kind<>&unit)) or (c.Kind=TClassKind.&record) then
    begin
      c.Renderinfo.Position := length(sl.Text)+1;
      sl.Add(c.ToPascalDeclaration);
      c.Renderinfo.Position := length(sl.Text)-c.Renderinfo.Position;
    end;


 if GlobalVars.Count>0 then
   sl.Add(GlobalVars.ToPascal(true));


  if length(self.GlobalArrays1D)>0 then
  begin
    sl.Add('const // 1d arrays');
    for ar in self.GlobalArrays1D do
    begin
      ar.Renderinfo.Position := sl.Text.Length+1;
      sl.Add(ar.ToPascal);
      ar.Renderinfo.Length   := sl.Text.Length - ar.Renderinfo.Position ;
    end;
  end;

  if length(self.GlobalArrays2D)>0 then
  begin
    sl.Add('const // 2d arrays');
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
    for m in c.Methods do
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
      name,
      length(Items)-1,
      itemType,
      elms
    ]);

end;

{ TLoop }

function TLoop.toPascal: string;
var v:string;t:integer;
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

  Result := Format('for %s := %s to %s do', [
              IndexerVar.Name, // for XX
              StartVal,        // :=  XX
              v                // to  XX
              ]);

end;


{ TUsesList }

constructor TUsesList.Create(aUnit: TPascalUnit);
begin
  self.&Unit := aUnit;
end;

procedure TUsesList.AddUnit(s: string);
var t:string;
begin
  if &unit<>nil then
    if SameText(s,&Unit.Name) then
      exit;

  for t in self.Units do
    if SameText(s,t) then
      Exit;

  self.Units := self.Units + [s];
end;

function TUsesList.getCount: integer;
begin
  Result := length(Self.Units)
end;

function TUsesList.ToPascal: string;
begin
  Result := '';
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
  if name='' then
  begin
    setlength(a,length(Items));
    for I := 0 to high(Items) do
      a[I] := Items[I];

    Exit('  '+''.Join(';'+sLineBreak+'  ',a )+sLineBreak);
  end;


  setlength(a,length(Items));
  for I := 0 to high(Items) do
    a[I] := Items[I];

  Result := '  '+ Esc(name) +' = (' + sLineBreak +
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

  Result :=
    format( '  %s : array[0..%d,0..%d] of %s = ('+sLineBreak+'    ',[
      name,
      length(Items)-1,
      length(Items[0])-1,
      itemType
    ]);

  Result := Result + ''.join(','+sLineBreak+'    ',b);

  Result := Result + ');'+sLineBreak;
end;

end.
