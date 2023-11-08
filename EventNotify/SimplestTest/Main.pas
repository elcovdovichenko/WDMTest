unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DevIEnum, Thread;

type
  TForm1 = class(TForm)
    bnEvent: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bnEventClick(Sender: TObject);
  private
    { Private declarations }
    Interfaces: TDeviceInterfaceClassEnumerator;
    hDevice: THandle;
    EventThread: TEventThread;
    data: integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var DeviceName: string; i, n: integer;
begin

  Interfaces:=TDeviceInterfaceClassEnumerator.Create(@EventsIID);
  if Interfaces.Handle = INVALID_HANDLE_VALUE
  then begin
       OutputDebugString('Device Interface Class is not exist'#13#10);
       Interfaces.Free;
       Exit;
       end;

  i:=Interfaces.Count;
  OutputDebugString(PChar(IntToStr(i)+' device interface(s) is found'#13#10));
  if i > 0
  then for n:=0 to i-1 do OutputDebugString(PChar(Interfaces[n]+#13#10))
  else Exit;

  DeviceName:=Interfaces[0];
  hDevice:=CreateFile(PChar(DeviceName), 0, 0, NIL, OPEN_EXISTING, 0, 0);

  if hDevice = INVALID_HANDLE_VALUE
  then begin
       OutputDebugString(PChar('Error on open the device: '+IntToStr(GetLastError)+#13#10));
       exit;
       end
  else OutputDebugString('Device in process is opened'#13#10);

  EventThread:=TEventThread.Create(DeviceName);
  data:=0;

end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  OutputDebugString('Device in process is closed'#13#10);
  CloseHandle(hDevice);
end;

procedure TForm1.bnEventClick(Sender: TObject);
var count: DWord;
begin
  inc(data);

  if DeviceIOControl(hDevice, IOCTL_MANUAL_ACTIVITY, @data, 4, NIL, 0, count, NIL)
  then begin
       bnEvent.Caption:=IntToStr(data);
       OutputDebugString(PChar('Event='+IntToStr(data)+#13#10));
       end
  else OutputDebugString('Event DeviceIOControl ERROR'#13#10);
end;

end.
