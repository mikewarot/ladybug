unit ladybug_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, BasicCompiler;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonInterpret: TButton;
    ButtonTokenize: TButton;
    ButtonParse: TButton;
    ButtonTestInterpret: TButton;
    KeyWords: TListBox;
    MainMenu1: TMainMenu;
    ProgramLines: TMemo;
    MemoOutput: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    DisplayWhitespace: TMenuItem;
    SourceCode: TMemo;
    procedure ButtonInterpretClick(Sender: TObject);
    procedure ButtonTestInterpretClick(Sender: TObject);
    procedure ButtonParseClick(Sender: TObject);
    procedure ButtonTokenizeClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

Procedure ShowOutput(S : String);
begin
  form1.MemoOutput.Append(s);
end;

procedure TForm1.ButtonParseClick(Sender: TObject);
var
  c     : char;
begin
  SetSourceCode(SourceCode.Text);
  MemoOutput.Clear;
  c := GetCharacter;
  While C <> ^Z do
  begin
    MemoOutput.Lines.Append('Char ['+expanded(c)+']');
    C := GetCharacter;
  end;
end;

procedure TForm1.ButtonTestInterpretClick(Sender: TObject);
var
  s : string;
  t,x : ttoken;
  i : integer;
begin
  SetSourceCode(SourceCode.Text);
  MemoOutput.Clear;
  Repeat
    GetToken(T);
    Case T.Kind of
      Word         : Case T.Name.ToUpper of
                       'BYE'   : Application.Terminate;
                       'CLEAR' : ProgramLines.Clear;
                       'LIST'  : MemoOutput.Text := ProgramLines.Text;
                       'PRINT' : begin
                                   S := '';
                                   repeat
                                     GetToken(X);
                                     Case X.Kind of
                                       GotString,
                                       Number               : S := S + X.Name;
                                     else
                                       s := s + ' ';
                                     end;
                                   until X.Kind in [EOL,EOF];
                                   MemoOutput.Append(S);
                                 end;
                     else
                       MemoOutput.Append('Unhandled WORD : '+T.Name);
                     end;  // CaseT.Name.ToUpper
      Number       : begin
                       S := T.Name;
                       Repeat
                         GetToken(T);
                         If T.Kind <> EOL then
                           S := S + T.Name;
                       Until T.Kind in [EOL,EOF];
                       ProgramLines.Append(S);
                     end;
      EOL,EOF      : // nothing
    else
      MemoOutput.Append('Unhandled Token '+StateName[T.Kind] + ' ['+T.Name+']');
    end; // case T.Kind
  Until T.Kind = EOF;
end;

procedure TForm1.ButtonInterpretClick(Sender: TObject);
begin
  MemoOutput.Clear;
  Interpret(SourceCode.Text);
end;

procedure TForm1.ButtonTokenizeClick(Sender: TObject);
var
  s : string;
  t : ttoken;
  i : integer;
begin
  SetSourceCode(SourceCode.Text);
  MemoOutput.Clear;
  GetToken(T);
  While (T.Kind <> EOF) do
  begin
    Case T.Kind of
      WhiteSpace,
      EOL,EOF      : If Form1.DisplayWhitespace.Checked then
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

initialization
  BasicCompiler.StringOut := @ShowOutput;
end.

