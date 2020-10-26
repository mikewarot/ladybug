unit ladybug_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonParse: TButton;
    MemoOutput: TMemo;
    SourceCode: TMemo;
    procedure ButtonParseClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonParseClick(Sender: TObject);
var
  i,j,k : integer;
  c     : char;
  expanded     : string;
  sourcebuffer : string;
begin
  SourceBuffer := SourceCode.Text;
  MemoOutput.Clear;
  for i := 1 to length(SourceBuffer) do
  begin
    c := sourcebuffer[i];
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
  end;
end;

end.

