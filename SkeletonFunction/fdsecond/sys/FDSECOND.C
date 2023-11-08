/*++

Copyright (c) 2001  ELCO  All Rights Reserved

Module Name:

    fdsecond.c

Abstract:

    This module shows how to write a simplest function driver
    for Windows 2000.

Environment:

    Kernel mode

Revision History:

    Vyacheslav Vdovichenko Jan 22 2001

--*/
#include <wdm.h>  //ntddk.h>
#include <initguid.h>
#include "..\inc\public.h"
#include "fdsecond.h"

#ifdef ALLOC_PRAGMA
#pragma alloc_text (INIT, DriverEntry)
#pragma alloc_text (PAGE, FD_AddDevice)
#pragma alloc_text (PAGE, FD_Dispatch)
#pragma alloc_text (PAGE, FD_DispatchPnp)
#pragma alloc_text (PAGE, FD_DispatchPower)
#pragma alloc_text (PAGE, FD_Unload)
#endif


NTSTATUS
DriverEntry(
    IN PDRIVER_OBJECT  DriverObject,
    IN PUNICODE_STRING RegistryPath
    )
/*++

Routine Description:

    Installable driver initialization entry point.
    This entry point is called directly by the I/O system.

Arguments:

    DriverObject - pointer to the driver object

    RegistryPath - pointer to a unicode string representing the path,
                   to driver-specific key in the registry.

Return Value:

    STATUS_SUCCESS if successful,
    STATUS_UNSUCCESSFUL otherwise.

--*/
{
    NTSTATUS            status = STATUS_SUCCESS;

    UNREFERENCED_PARAMETER (RegistryPath);

    DebugPrint (("Entered the Driver Entry\n"));

    //
    // Create dispatch points for the IRPs.
    //

    DriverObject->MajorFunction[IRP_MJ_PNP]            = FD_DispatchPnp;
    DriverObject->MajorFunction[IRP_MJ_POWER]          = FD_DispatchPower;
    DriverObject->MajorFunction[IRP_MJ_CREATE]         = FD_Dispatch;
    DriverObject->MajorFunction[IRP_MJ_CLOSE]          = FD_Dispatch;
    DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = FD_Dispatch;
    DriverObject->DriverExtension->AddDevice           = FD_AddDevice;
    DriverObject->DriverUnload                         = FD_Unload;

    return status;
}


NTSTATUS
FD_AddDevice(
    IN PDRIVER_OBJECT DriverObject,
    IN PDEVICE_OBJECT PhysicalDeviceObject
    )
/*++

Routine Description:

    The Plug & Play subsystem is handing us a brand new PDO, for which we
    (by means of INF registration) have been asked to provide a driver.

    We need to determine if we need to be in the driver stack for the device.
    Create a function device object to attach to the stack
    Initialize that device object
    Return status success.

    Remember: We can NOT actually send ANY non pnp IRPS to the given driver
    stack, UNTIL we have received an IRP_MN_START_DEVICE.

Arguments:

    DeviceObject - pointer to a device object.

    PhysicalDeviceObject -  pointer to a device object created by the
                            underlying bus driver.

Return Value:

    NT status code.

--*/
{
    NTSTATUS                status;
    PDEVICE_OBJECT          deviceObject = NULL;
    PDEVICE_EXTENSION       deviceExtension;
    UNICODE_STRING          devname;

    PAGED_CODE ();

    RtlInitUnicodeString(&devname, L"\\DosDevices\\FDSECOND");

    //
    // Create the device object.
    //

    status = IoCreateDevice (DriverObject,              // DriverObject
                             sizeof (DEVICE_EXTENSION), // DeviceExtentionSize
                             &devname, //NULL,                      // No DeviceName
                             FILE_DEVICE_UNKNOWN,       // Unknown DeviceType
                             FILE_DEVICE_SECURE_OPEN,   // DeviceCharacteristics
                             FALSE,                     // Exclusive = FALSE in WDM
                             &deviceObject);            // DeviceObject


    if (!NT_SUCCESS (status)) {
        //
        // Returning failure here prevents the entire stack from functioning,
        // but most likely the rest of the stack will not be able to create
        // device objects either, so it is still OK.
        //
        return status;
    }

    DebugPrint (("AddDevice PDO (0x%x) FDO (0x%x)\n",
                 PhysicalDeviceObject, deviceObject));

    deviceExtension = (PDEVICE_EXTENSION) deviceObject->DeviceExtension;

    deviceExtension->NextLowerDriver = IoAttachDeviceToDeviceStack (
                                       deviceObject,
                                       PhysicalDeviceObject);
    //
    // Failure for attachment is an indication of a broken plug & play system.
    //

    if(NULL == deviceExtension->NextLowerDriver) {

        IoDeleteDevice(deviceObject);
        return STATUS_UNSUCCESSFUL;
    }

    //
    // Tell the Plug & Play system that this device will need an interface
    //

    status = IoRegisterDeviceInterface (
                PhysicalDeviceObject,
                (LPGUID) &GUID_FUNCTION_DEVICE_INTERFACE_CLASS,
                NULL,
                &deviceExtension->InterfaceName);

    if (!NT_SUCCESS (status)) {
        DebugPrint ((
            "AddDevice: IoRegisterDeviceInterface failed (%x)\n", status));
        IoDetachDevice(deviceExtension->NextLowerDriver);
        IoDeleteDevice (deviceObject);
        return status;
    }

    deviceObject->Flags |= deviceExtension->NextLowerDriver->Flags &
                            (DO_BUFFERED_IO | DO_POWER_PAGABLE);


    deviceObject->DeviceType = deviceExtension->NextLowerDriver->DeviceType;

    deviceObject->Characteristics =
                          deviceExtension->NextLowerDriver->Characteristics;

    deviceExtension->Self = deviceObject;

    deviceExtension->UnderlyingPDO = PhysicalDeviceObject;

    //
    // Set the initial state of the FDO
    //

    INITIALIZE_PNP_STATE(deviceExtension);

    DebugPrint(("AddDevice: %x to %x->%x \n", deviceObject,
                       deviceExtension->NextLowerDriver,
                       PhysicalDeviceObject));

    deviceObject->Flags &= ~DO_DEVICE_INITIALIZING;

    return STATUS_SUCCESS;

}

NTSTATUS
FD_Dispatch(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
/*++

Routine Description:

    Common entrypoint for all Io Request Packets

Arguments:

    DeviceObject - pointer to a device object.
    Irp - Io Request Packet

Return Value:

    STATUS_SUCCESS if the IRP was processed successfully, otherwise an error
    indicating the reason for failure.

--*/

{
    PIO_STACK_LOCATION          irpStack;
    NTSTATUS                    status = STATUS_SUCCESS;
    ULONG                       info = 0;
    ULONG                       cbin, cbout, code;
    PULONG                      poperation;

    PAGED_CODE();

    irpStack = IoGetCurrentIrpStackLocation(Irp);

    DebugPrint(("Driver DO %s IRP:0x%x \n",
                PnPMinorFunctionString(irpStack->MajorFunction), Irp));

    switch (irpStack->MajorFunction)
    {
        case IRP_MJ_CREATE:
        case IRP_MJ_CLOSE:
            // We don't need any special processing on open/close so we'll
            // just return success.
            break;

        case IRP_MJ_DEVICE_CONTROL:
	    cbin  = irpStack->Parameters.DeviceIoControl.InputBufferLength;
	    cbout = irpStack->Parameters.DeviceIoControl.OutputBufferLength;
	    code  = irpStack->Parameters.DeviceIoControl.IoControlCode;


            DebugPrint(("Driver IOCTL:%u IRP:0x%x \n", code, Irp));

	    switch (code)
	    {					// process request

	    case IOCTL_GET_RESULT_OPERATION:	// code == 0x800
                 {
	         if ((cbout < sizeof(ULONG)) || (cbin < sizeof(ULONG)))
		   {				// not enough output data
	             status = STATUS_INVALID_BUFFER_SIZE;
                     break;
                   }

		 poperation = Irp->AssociatedIrp.SystemBuffer;
		 info = sizeof(ULONG);
		 *poperation *= 777;
		 break;
		 }

	    default:
		 status = STATUS_INVALID_DEVICE_REQUEST;
		 break;

	    }						// process request
            break;

        default:
            status = STATUS_NOT_IMPLEMENTED;
            break;
    }

    //
    // We're done with I/O request.  Record the status of the I/O action.
    //
    Irp->IoStatus.Status = status;
    Irp->IoStatus.Information = info;
    IoCompleteRequest (Irp, IO_NO_INCREMENT);

    return status;
}

FD_CompletionRoutine(
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP             Irp,
    IN PVOID            Context
    )
/*++
Routine Description:
    A completion routine for use when calling the lower device objects to
    which our deviceobject is attached.

Arguments:

    DeviceObject - Pointer to deviceobject
    Irp          - Pointer to a PnP Irp.
    Context      - Pointer to an event object    
Return Value:

    NT Status is returned.

--*/

{
    UNREFERENCED_PARAMETER(DeviceObject);

    if (Irp->PendingReturned) {
        IoMarkIrpPending(Irp);
    }

    //
    // We could switch on the major and minor functions of the IRP to perform
    // different functions, but we know that Context is an event that needs
    // to be set.
    //

    KeSetEvent((PKEVENT) Context, IO_NO_INCREMENT, FALSE);

    //
    // Allows the caller to use the IRP after it is completed
    //
    return STATUS_MORE_PROCESSING_REQUIRED;
}


NTSTATUS
FD_DispatchPnp (
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
/*++

Routine Description:

    The plug and play dispatch routines.

    Most of these requests the driver will completely ignore.
    In all cases it must pass on the IRP to the lower driver.

Arguments:

   DeviceObject - pointer to a device object.

   Irp - pointer to an I/O Request Packet.

Return Value:

      NT status code

--*/
{
    PDEVICE_EXTENSION           deviceExtension;
    PIO_STACK_LOCATION          irpStack;
    NTSTATUS                    status;
    KEVENT                      event;

    PAGED_CODE();

    deviceExtension = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;
    irpStack = IoGetCurrentIrpStackLocation(Irp);

    DebugPrint(("Driver DO %s IRP:0x%x \n",
                PnPMinorFunctionString(irpStack->MinorFunction), Irp));

    status = STATUS_SUCCESS;

    switch (irpStack->MinorFunction) {
    case IRP_MN_START_DEVICE:

        //
        // The device is starting.
        //
        // We cannot touch the device (send it any non pnp irps) until a
        // start device has been complited.
        //
        IoCopyCurrentIrpStackLocationToNext(Irp);
        KeInitializeEvent(&event,
                          NotificationEvent,
                          FALSE
                          );

        IoSetCompletionRoutine(Irp,
                               (PIO_COMPLETION_ROUTINE) FD_CompletionRoutine,
                               &event,
                               TRUE,
                               TRUE,
                               TRUE); // No need for Cancel

        status = IoCallDriver(deviceExtension->NextLowerDriver, Irp);

        if (STATUS_PENDING == status) {
            KeWaitForSingleObject(
               &event,
               Executive, // Waiting for reason of a driver
               KernelMode, // Waiting in kernel mode
               FALSE, // No alert
               NULL); // No timeout
        }

        if (NT_SUCCESS(status) && NT_SUCCESS(Irp->IoStatus.Status)) {
            //
            // Enable the device interface. Return status
            // STATUS_OBJECT_NAME_EXISTS means we are enabling the interface
            // that was already enabled, which could happen if the device
            // is stopped and restarted for resource rebalancing.
            //
            status = IoSetDeviceInterfaceState(&deviceExtension->InterfaceName, TRUE);

            if (!NT_SUCCESS (status)){
                DebugPrint(("IoSetDeviceInterfaceState failed: 0x%x\n", status));
                return status;
            }
            //
            // As we are successfully now back from our start device
            // we can do work.
            //
            SET_NEW_PNP_STATE(deviceExtension, Started);
        }

        //
        // We must now complete the IRP, since we stopped it in the
        // completion routine with STATUS_MORE_PROCESSING_REQUIRED.
        //
        Irp->IoStatus.Status = status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        return status;


    case IRP_MN_REMOVE_DEVICE:

        //
        // Disable the device interface.
        //

        status = IoSetDeviceInterfaceState(&deviceExtension->InterfaceName, FALSE);

        if (!NT_SUCCESS (status)) {
            DebugPrint(("IoSetDeviceInterfaceState failed: 0x%x\n", status));
        }

        IoSkipCurrentIrpStackLocation(Irp);

        status = IoCallDriver(deviceExtension->NextLowerDriver, Irp);

        SET_NEW_PNP_STATE(deviceExtension, Deleted);

        IoDetachDevice(deviceExtension->NextLowerDriver);
        IoDeleteDevice(DeviceObject);
        return status;


    case IRP_MN_QUERY_STOP_DEVICE:

        SET_NEW_PNP_STATE(deviceExtension, StopPending);
        status = STATUS_SUCCESS;
        break;

    case IRP_MN_CANCEL_STOP_DEVICE:

        //
        // Check to see whether you have received cancel-stop
        // without first receiving a query-stop. This could happen if someone
        // above us fails a query-stop and passes down the subsequent
        // cancel-stop.
        //

        if(StopPending == deviceExtension->DevicePnPState)
        {
            //
            // We did receive a query-stop, so restore.
            //
            RESTORE_PREVIOUS_PNP_STATE(deviceExtension);
        }
        status = STATUS_SUCCESS; // We must not fail this IRP.
        break;

    case IRP_MN_STOP_DEVICE:

        SET_NEW_PNP_STATE(deviceExtension, StopPending);
        status = STATUS_SUCCESS;
        break;

    case IRP_MN_QUERY_REMOVE_DEVICE:

        SET_NEW_PNP_STATE(deviceExtension, RemovePending);
        status = STATUS_SUCCESS;
        break;

    case IRP_MN_SURPRISE_REMOVAL:

        //
        // Disable the device interface.
        //

        status = IoSetDeviceInterfaceState(&deviceExtension->InterfaceName, FALSE);

        if (!NT_SUCCESS (status)) {
            DebugPrint(("IoSetDeviceInterfaceState failed: 0x%x\n", status));
        }
        SET_NEW_PNP_STATE(deviceExtension, SurpriseRemovePending);
        status = STATUS_SUCCESS;
        break;

    case IRP_MN_CANCEL_REMOVE_DEVICE:

        //
        // Check to see whether you have received cancel-remove
        // without first receiving a query-remove. This could happen if
        // someone above us fails a query-remove and passes down the
        // subsequent cancel-remove.
        //

        if(RemovePending == deviceExtension->DevicePnPState)
        {
            //
            // We did receive a query-remove, so restore.
            //
            RESTORE_PREVIOUS_PNP_STATE(deviceExtension);
        }
        status = STATUS_SUCCESS; // We must not fail this IRP.
        break;


    default:
        //
        // If you don't handle any IRP you must leave the
        // status as is.
        //
        status = Irp->IoStatus.Status;

        break;
    }


    //
    // We're done with I/O request.  Record the status of the I/O action.
    //
    Irp->IoStatus.Status = status;
    Irp->IoStatus.Information = 0;
    IoCompleteRequest (Irp, IO_NO_INCREMENT);

    return status;
}


NTSTATUS
FD_DispatchPower(
    IN PDEVICE_OBJECT    DeviceObject,
    IN PIRP              Irp
    )
/*++

Routine Description:

    This routine is the dispatch routine for power irps.

Arguments:

    DeviceObject - Pointer to the device object.

    Irp - Pointer to the request packet.

Return Value:

    NT Status code
--*/
{
    PDEVICE_EXTENSION   deviceExtension;

    PAGED_CODE ();

    deviceExtension = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;
    PoStartNextPowerIrp(Irp);
    IoSkipCurrentIrpStackLocation(Irp);
    return PoCallDriver(deviceExtension->NextLowerDriver, Irp);
}


VOID
FD_Unload(
    IN PDRIVER_OBJECT DriverObject
    )
/*++

Routine Description:

    Free all the allocated resources in DriverEntry, etc.

Arguments:

    DriverObject - pointer to a driver object.

Return Value:

    VOID.

--*/
{
    PAGED_CODE ();

    //
    // The device object(s) should be NULL now
    // (since we unload, all the devices objects associated with this
    // driver must be deleted.
    //
    ASSERT(DriverObject->DeviceObject == NULL);
    
    //
    // We should not be unloaded until all the devices we control 
    // have been removed from our queue.
    //
    DebugPrint (("unload\n"));

    return;
}

#if DBG

PCHAR
PnPMinorFunctionString (
    UCHAR MinorFunction
)
{
    switch (MinorFunction)
    {
        case IRP_MN_START_DEVICE:
            return "IRP_MN_START_DEVICE";
        case IRP_MN_QUERY_REMOVE_DEVICE:
            return "IRP_MN_QUERY_REMOVE_DEVICE";
        case IRP_MN_REMOVE_DEVICE:
            return "IRP_MN_REMOVE_DEVICE";
        case IRP_MN_CANCEL_REMOVE_DEVICE:
            return "IRP_MN_CANCEL_REMOVE_DEVICE";
        case IRP_MN_STOP_DEVICE:
            return "IRP_MN_STOP_DEVICE";
        case IRP_MN_QUERY_STOP_DEVICE:
            return "IRP_MN_QUERY_STOP_DEVICE";
        case IRP_MN_CANCEL_STOP_DEVICE:
            return "IRP_MN_CANCEL_STOP_DEVICE";
        case IRP_MN_QUERY_DEVICE_RELATIONS:
            return "IRP_MN_QUERY_DEVICE_RELATIONS";
        case IRP_MN_QUERY_INTERFACE:
            return "IRP_MN_QUERY_INTERFACE";
        case IRP_MN_QUERY_CAPABILITIES:
            return "IRP_MN_QUERY_CAPABILITIES";
        case IRP_MN_QUERY_RESOURCES:
            return "IRP_MN_QUERY_RESOURCES";
        case IRP_MN_QUERY_RESOURCE_REQUIREMENTS:
            return "IRP_MN_QUERY_RESOURCE_REQUIREMENTS";
        case IRP_MN_QUERY_DEVICE_TEXT:
            return "IRP_MN_QUERY_DEVICE_TEXT";
        case IRP_MN_FILTER_RESOURCE_REQUIREMENTS:
            return "IRP_MN_FILTER_RESOURCE_REQUIREMENTS";
        case IRP_MN_READ_CONFIG:
            return "IRP_MN_READ_CONFIG";
        case IRP_MN_WRITE_CONFIG:
            return "IRP_MN_WRITE_CONFIG";
        case IRP_MN_EJECT:
            return "IRP_MN_EJECT";
        case IRP_MN_SET_LOCK:
            return "IRP_MN_SET_LOCK";
        case IRP_MN_QUERY_ID:
            return "IRP_MN_QUERY_ID";
        case IRP_MN_QUERY_PNP_DEVICE_STATE:
            return "IRP_MN_QUERY_PNP_DEVICE_STATE";
        case IRP_MN_QUERY_BUS_INFORMATION:
            return "IRP_MN_QUERY_BUS_INFORMATION";
        case IRP_MN_DEVICE_USAGE_NOTIFICATION:
            return "IRP_MN_DEVICE_USAGE_NOTIFICATION";
        case IRP_MN_SURPRISE_REMOVAL:
            return "IRP_MN_SURPRISE_REMOVAL";
            
        default:
            return "IRP_MN_?????";
    }
}

#endif

