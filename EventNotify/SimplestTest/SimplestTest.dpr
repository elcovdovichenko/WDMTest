program SimplestTest;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  thread in 'thread.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
