program SinchroTest;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  sinchro in '..\device\sinchro.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
