unit thread;

interface

uses
  Windows, Messages, SysUtils, ExtCtrls, Classes;

const
  { evnotify interface GUID }
  EventsIID: TGUID = '{04B40308-3F0C-4EF3-BAF0-DCD8BD06DA53}';
  { IOCTLs }
  IOCTL_MANUAL_ACTIVITY = $222000;
  IOCTL_TIMER_ACTIVITY = $222004;
  IOCTL_MANUAL_EVENT = $222008;
  IOCTL_TIMER_EVENT = $22200C;

type
  TEventThread = class(TThread)
  private
    hDevice: THandle;
    DevName: string;
  protected
    procedure Execute; override;
  public
    constructor Create(Name: string);
  end;

implementation

{ TEventThread }
constructor TEventThread.Create(Name: string);
begin
  inherited Create(False);
  DevName:=Name;
  FreeOnTerminate:=True;
end;

procedure TEventThread.Execute;
var complite: integer; count: DWord;
begin
  OutputDebugString('Execute Entry'#13#10);

  hDevice:=CreateFile(PChar(DevName), 0, 0, NIL, OPEN_EXISTING, 0, 0);

  if hDevice = INVALID_HANDLE_VALUE
  then begin
       OutputDebugString(PChar('Error on open the device: '+IntToStr(GetLastError)+#13#10));
       exit;
       end
  else OutputDebugString('Device in thread is opened'#13#10);

  while TRUE do
  if DeviceIOControl(hDevice, IOCTL_MANUAL_EVENT, NIL, 0, @complite, 4, count, NIL)
  then OutputDebugString(PChar('Execute Exit Successfully ='+IntToStr(complite)+#13#10))
  else OutputDebugString('Execute Exit DeviceIOControl ERROR'#13#10);

  CloseHandle(hDevice);
end;

end.




