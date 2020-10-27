unit BasicCompiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  ParseState = (Starting,Number,Word,WhiteSpace,Unknown,Done,GetString,GotString,EatDone,EOL,EOF);
Const
  StateName : Array[ParseState] of String = ('Starting','Number','Word','WhiteSpace','Unknown','Done','Unterminated String','String','EatDone','EOL','EOF');
Type
  ttoken = record
    Name : String;
    Kind : ParseState;
  end;

var
  SourceBuffer : string = '';
  SourcePos    : Integer = 1;

  function GetCharacter : Char;

  function PeekCharacter : Char;

  function Expanded(C : Char):String;

implementation

function GetCharacter : Char;
begin
  If SourcePos <= Length(SourceBuffer) then
  begin
    GetCharacter := SourceBuffer[SourcePos];
    Inc(SourcePos);
  end
  else
    GetCharacter := ^Z; // control-z if we're at end
end;

function PeekCharacter : Char;
begin
  If SourcePos <= Length(SourceBuffer) then
    PeekCharacter := SourceBuffer[SourcePos]
  else
    PeekCharacter := ^Z;
end;

function Expanded(C : Char):String;
var
  s : string;
begin
  s := '';
  case c of
    #0   : s := '#0';
    #27  : s := 'ESC';
    #1..#26  : s := '^' + char(ord(c)+64);
    #28..#31 : s := '^' + ord(c).ToString;
    #127 : s := 'DEL';
  else
    s := c;
  end; // case
  Expanded := S;
end;



end.

