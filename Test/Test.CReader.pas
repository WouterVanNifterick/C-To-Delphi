unit Test.CReader;

interface

uses
  WvN.Pascal.CReader,
  WvN.Pascal.Model,
  DUnitX.TestFramework;

type

  [TestFixture]
  CReaderTests = class(TObject)
  public
    onprogress:TOnProgress;
    PascalUnit:TPascalUnit;
    t:string;

    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure TestThatNameIsUsed;
    // Test with TestCase Attribute to supply parameters.
    [Test]
    [TestCase('TestInt','int,Integer')]
    [TestCase('TestLongLong','long long,int64')]
    [TestCase('TestLongLong','char,Byte')]
    procedure Test2(CType:string;PascalType:string);
  end;

implementation

procedure CReaderTests.Setup;
begin
  onprogress := procedure(progress:double;const text:string)
                begin
                  System.WriteLn(progress:2:2, ' ',Text);
                end;
  PascalUnit := TPascalUnit.Create(nil);

end;

procedure CReaderTests.TearDown;
begin
  PascalUnit.Free;
end;

procedure CReaderTests.TestThatNameIsUsed;
var C:string;
begin
  C := '';
  WvN.Pascal.CReader.c_to_pas(C,t,'test',onProgress,PascalUnit);
  Assert.AreEqual('test',PascalUnit.Name);
end;

procedure CReaderTests.Test2(CType:string; PascalType:string);
var C:string;
begin
  C := CType +' xxx = 14;';
  WvN.Pascal.CReader.c_to_pas(C,t,'',onProgress,PascalUnit);
  Assert.AreEqual(PascalType, TVariable(PascalUnit.GlobalVars[0]).&Type );
end;

initialization
  TDUnitX.RegisterTestFixture(CReaderTests);
end.
