unit ladybug_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonTokenize: TButton;
    ButtonParse: TButton;
    ButtonInterpret: TButton;
    KeyWords: TListBox;
    MainMenu1: TMainMenu;
    ProgramLines: TMemo;
    MemoOutput: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    DisplayWhitespace: TMenuItem;
    SourceCode: TMemo;
    procedure ButtonInterpretClick(Sender: TObject);
    procedure ButtonParseClick(Sender: TObject);
    procedure ButtonTokenizeClick(Sender: TObject);
  private

  public

  end;

Type
  ParseState = (Starting,Number,Word,WhiteSpace,Unknown,Done,DoubleQuoteString,EatDone);
Const
  StateName : Array[ParseState] of String = ('Starting','Number','Word','WhiteSpace','Unknown','Done','"String"','EatDone');
Type
  ttoken = record
    Name : String;
    Kind : ParseState;
  end;

const
  ESC  = #27;
  EOF  = #26;
var
  Form1: TForm1;
  SourceBuffer : string = '';
  SourcePos    : Integer = 1;

implementation

{$R *.lfm}

{ TForm1 }

function GetCharacter : Char;
begin
  If SourcePos <= Length(SourceBuffer) then
  begin
    GetCharacter := SourceBuffer[SourcePos];
    Inc(SourcePos);
  end
  else
    GetCharacter := EOF; // control-z if we're at end
end;

function PeekCharacter : Char;
begin
  If SourcePos <= Length(SourceBuffer) then
    PeekCharacter := SourceBuffer[SourcePos]
  else
    PeekCharacter := EOF;
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

procedure GetToken(Var T : TToken);
var
  S : string;
  state,nextstate : ParseState;
  c : char;
begin
  T.Kind:= Unknown;
  T.Name:= '';
  State := Starting;
  NextState := Done;  // default to done, for safety
  S := '';
  repeat
    C := PeekCharacter;
    Case State of
      Starting    : Case C of
                      '0'..'9' : NextState := Number;
                      'A'..'Z',
                      'a'..'z',
                      '_'      : NextState := Word;
                      #9,#10,
                      #13,' '  : NextState := WhiteSpace;
                      '"'      : NextState := DoubleQuoteString;
                    else
                      NextState := Unknown;
                    end;
      Number      : Case C of
                      '0'..'9' : NextState := Number;
                    Else
                      NextState := Done;
                    end;
      Word        : Case C of
                      'A'..'Z',
                      'a'..'z',
                      '0'..'9',
                      '_'       : NextState := Word;
                    Else
                      NextState := Done;
                    end;
      WhiteSpace  : Case C of
                      #9,#10,#13,' ' : NextState := WhiteSpace;
                    else
                      NextState := Done;
                    end;
      DoubleQuoteString :
                    Case C of
                      EOF : NextState := Done;
                      '"' : NextState := EatDone;
                    else
                      NextState := DoubleQuoteString;
                    end;

      Unknown     : NextState := Done;
    else
      Form1.MemoOutput.Append('Unknown Token : ['+S+']');
      NextState := Done;
    end; // case State
    If NextState <> Done then
      S := S + GetCharacter;

    T.Kind:= State;
    T.Name:=S;
    State := NextState;
  until State in [Done,EatDone];
end;

procedure TForm1.ButtonParseClick(Sender: TObject);
var
  c     : char;
  expanded     : string;
begin
  SourceBuffer := SourceCode.Text;
  SourcePos    := 1;
  MemoOutput.Clear;
  c := GetCharacter;
  While C <> EOF do
  begin
    expanded := '';
    case c of
      #0   : expanded := '#0';
      #27  : expanded := 'ESC';
      #1..#26  : expanded := '^' + char(ord(c)+64);
      #28..#31 : expanded := '^' + ord(c).ToString;
      #127 : expanded := 'DEL';
    else
      expanded := c;
    end; // case
    MemoOutput.Lines.Append('Char ['+expanded+']');
    C := GetCharacter;
  end;
end;

procedure TForm1.ButtonInterpretClick(Sender: TObject);
var
  s : string;
  t : ttoken;
  i : integer;
begin
  SourceBuffer := SourceCode.Text;
  SourcePos    := 1;
  MemoOutput.Clear;
  GetToken(T);
  Case T.Kind of
    Word         : Case T.Name.ToUpper of
                     'BYE'  : Application.Terminate;
                     'LIST' : MemoOutput.Text := ProgramLines.Text;
                   else
                     MemoOutput.Append('Unhandled WORD : '+T.Name);
                   end;  // CaseT.Name.ToUpper
  end; // case T.Kind
end;

procedure TForm1.ButtonTokenizeClick(Sender: TObject);
var
  s : string;
  t : ttoken;
  i : integer;
begin
  SourceBuffer := SourceCode.Text;
  SourcePos    := 1;
  MemoOutput.Clear;
  GetToken(T);
  While (T.Name <> EOF) do
  begin
    Case T.Kind of
      WhiteSpace   : If Form1.DisplayWhitespace.Checked then
                       MemoOutput.Lines.Append(StateName[T.Kind] + ' ['+T.Name+']');
      Word         : Begin
                       I := KeyWords.Items.IndexOf(T.Name);
                       If I >= 0 then
                         MemoOutput.Lines.Append('KEYWORD: '+KeyWords.Items[i])
                       Else
                         MemoOutput.Lines.Append('UNKNOWN KEYWORD: '+T.Name);
                     end;
    else
      MemoOutput.Lines.Append(StateName[T.Kind] + ' ['+T.Name+']');
    end;
    GetToken(T);
  end;
end;

end.

