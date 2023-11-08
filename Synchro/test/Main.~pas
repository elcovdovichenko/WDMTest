unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Sinchro, DevIEnum, ExtCtrls;

type
  TForm1 = class(TForm)
    bnLoad: TButton;
    bnClose: TButton;
    bnWait: TButton;
    Memo: TMemo;
    bnState: TButton;
    bnDelay: TButton;
    Edit: TEdit;
    Timer: TTimer;
    procedure bnLoadClick(Sender: TObject);
    procedure bnCloseClick(Sender: TObject);
    procedure bnWaitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bnStateClick(Sender: TObject);
    procedure bnDelayClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
    Device: TDevice;
    Interfaces: TDeviceInterfaceClassEnumerator;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.bnLoadClick(Sender: TObject);
var i, n: integer;
begin

   Interfaces:=TDeviceInterfaceClassEnumerator.Create(@SinchroIID);
   if Interfaces.Handle = INVALID_HANDLE_VALUE
   then begin
        Memo.Lines.Add('Device Interface Class is not exist');
        Interfaces.Free;
        Exit;
        end;

   i:=Interfaces.Count;
   Memo.Lines.Add(IntToStr(i)+' device interface(s) is found');
   if i > 0
   then for n:=0 to i-1 do Memo.Lines.Add(PChar(Interfaces[n]))
   else Exit;

   Device:=TDevice.Create(Interfaces[0]);
   if Device.HDevice = INVALID_HANDLE_VALUE
   then begin
        Memo.Lines.Add('Device is unable to open');
        Device.Free;
        Exit;
        end;

   bnLoad.Enabled:=False;
   bnClose.Enabled:=True;
   bnWait.Enabled:=True;
   bnState.Enabled:=True;
   bnDelay.Enabled:=True;
   Timer.Enabled:=True;

   Memo.Lines.Add('Device is ready');

end;

procedure TForm1.bnCloseClick(Sender: TObject);
begin

   bnLoad.Enabled:=True;
   bnClose.Enabled:=False;
   bnWait.Enabled:=False;
   bnState.Enabled:=False;
   bnDelay.Enabled:=False;
   Timer.Enabled:=False;

   Device.Free;
   Device:=nil;

   Interfaces.Free;
   Interfaces:=nil;

   Memo.Lines.Add('Driver is unloaded');

end;

procedure TForm1.bnWaitClick(Sender: TObject);
var i: integer;
begin
  i:=Device.Wait(StrToInt(Edit.Text));
  Memo.Lines.Add('Wait result is '+IntToStr(i));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Device:=nil;
  Interfaces:=nil;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin

  if Assigned(Interfaces)
  then Interfaces.Free;

  if Assigned(Device)
  then Device.Free;

end;

procedure TForm1.bnStateClick(Sender: TObject);
begin
  bnState.Caption:='State='+IntToStr(Device.State);
end;

procedure TForm1.bnDelayClick(Sender: TObject);
var i: integer;
begin
  i:=Device.Delay(StrToInt(Edit.Text));
  Memo.Lines.Add('Delay result is '+IntToStr(i));
end;

procedure TForm1.TimerTimer(Sender: TObject);
begin
  bnState.Caption:='State='+IntToStr(Device.State);
end;

end.
