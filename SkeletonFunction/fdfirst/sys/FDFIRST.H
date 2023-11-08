/*++

Copyright (c) 2001  ELCO  All Rights Reserved

Module Name:

    fdfirst.h

Abstract:

    This module shows how to write a simplest function driver
    for Windows2000.

Environment:

    Kernel mode

Revision History:

    Vyacheslav Vdovichenko Jan 12 2001

--*/

#if !defined(_FD_H_)
#define _FD_H_

#define DRIVERNAME "fdfirst.sys: "

#if DBG
#define DebugPrint(_x_) \
               DbgPrint (DRIVERNAME); \
               DbgPrint _x_;

#define TRAP() DbgBreakPoint()

#else
#define DebugPrint(_x_)
#define TRAP()
#endif

typedef enum _DEVICE_PNP_STATE {

    NotStarted = 0,         // Not started yet
    Started,                // Device has received the START_DEVICE IRP
    StopPending,            // Device has received the QUERY_STOP IRP
    Stopped,                // Device has received the STOP_DEVICE IRP
    RemovePending,          // Device has received the QUERY_REMOVE IRP
    SurpriseRemovePending,  // Device has received the SURPRISE_REMOVE IRP
    Deleted                 // Device has received the REMOVE_DEVICE IRP

} DEVICE_PNP_STATE;

#define INITIALIZE_PNP_STATE(_Data_)    \
        (_Data_)->DevicePnPState =  NotStarted;\
        (_Data_)->PreviousPnPState = NotStarted;

#define SET_NEW_PNP_STATE(_Data_, _state_) \
        (_Data_)->PreviousPnPState =  (_Data_)->DevicePnPState;\
        (_Data_)->DevicePnPState = (_state_);

#define RESTORE_PREVIOUS_PNP_STATE(_Data_)   \
        (_Data_)->DevicePnPState =   (_Data_)->PreviousPnPState;\


typedef struct _DEVICE_EXTENSION
{
    // A back pointer to the device object.
    PDEVICE_OBJECT  Self;

    // The underlying PDO
    PDEVICE_OBJECT  UnderlyingPDO;

    // The top of the stack before this driver was added.
    PDEVICE_OBJECT  NextLowerDriver;

    // current PnP state of the device
    DEVICE_PNP_STATE  DevicePnPState;

    // Remembers the previous pnp state
    DEVICE_PNP_STATE  PreviousPnPState;

    // The name returned from IoRegisterDeviceInterface
    UNICODE_STRING      InterfaceName;

} DEVICE_EXTENSION, *PDEVICE_EXTENSION;


PCHAR
PnPMinorFunctionString (
    UCHAR MinorFunction
);

NTSTATUS
FD_AddDevice(
    IN PDRIVER_OBJECT DriverObject,
    IN PDEVICE_OBJECT PhysicalDeviceObject
    );


NTSTATUS
FD_Dispatch(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
FD_DispatchPnp (
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
FD_DispatchPower(
    IN PDEVICE_OBJECT    DeviceObject,
    IN PIRP              Irp
    );

VOID
FD_Unload(
    IN PDRIVER_OBJECT DriverObject
    );

NTSTATUS
DriverEntry(
    IN PDRIVER_OBJECT  DriverObject,
    IN PUNICODE_STRING RegistryPath
    );

#endif

