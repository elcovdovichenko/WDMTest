// Main program for evnotify driver
// Copyright (C) 1999, 2000 by Walter Oney
// All rights reserved

#include "stddcls.h"
#include "driver.h"
#include <initguid.h>
#include "guids.h"

NTSTATUS AddDevice(IN PDRIVER_OBJECT DriverObject, IN PDEVICE_OBJECT pdo);
VOID DriverUnload(IN PDRIVER_OBJECT fdo);
NTSTATUS FilterGenericIdleControls(PDEVICE_OBJECT fdo, PIRP Irp);

// TODO If your driver uses more than one DEVQUEUE, remove the comment from the
// declaration of "morequeues" which follows, and adjust the array dimension to
// equal the number of queues you use minus one.

struct INIT_STRUCT : public _GENERIC_INIT_STRUCT {
//	QSIO morequeues[1];			// additional devqueue/sio pointers
	};

BOOLEAN win98 = FALSE;

UNICODE_STRING servkey;

///////////////////////////////////////////////////////////////////////////////

#pragma INITCODE

extern "C" NTSTATUS DriverEntry(IN PDRIVER_OBJECT DriverObject,
	IN PUNICODE_STRING RegistryPath)
	{							// DriverEntry

	// Insist that OS support at least the WDM level of the DDK we use

	if (!IoIsWdmVersionAvailable(1, 0))
		{
		KdPrint((DRIVERNAME " - Expected version of WDM (%d.%2.2d) not available\n", 1, 0));
		return STATUS_UNSUCCESSFUL;
		}

	// We require GENERIC.SYS 1.3 or later. If a version earlier than 1.3 is installed,
	// GenericGetVersion won't be exported, and this driver won't load in the first place.
	// Too bad I didn't think of including this function at the beginning!

	if (GenericGetVersion() < 0x00010003)
		{
		KdPrint((DRIVERNAME " - Required version (1.3) of GENERIC.SYS not installed\n"));
		return STATUS_UNSUCCESSFUL;
		}

	// See if we're running under Win98 or NT:

	win98 = IsWin98();


	// Save the name of the service key

	servkey.Buffer = (PWSTR) ExAllocatePool(PagedPool, RegistryPath->Length + sizeof(WCHAR));
	if (!servkey.Buffer)
		{
		KdPrint((DRIVERNAME " - Unable to allocate %d bytes for copy of service key name\n", RegistryPath->Length + sizeof(WCHAR)));
		return STATUS_INSUFFICIENT_RESOURCES;
		}
	servkey.MaximumLength = RegistryPath->Length + sizeof(WCHAR);
	RtlCopyUnicodeString(&servkey, RegistryPath);
	servkey.Buffer[RegistryPath->Length / 2] = 0;	// add a null terminator

	// Initialize function pointers

	DriverObject->DriverUnload = DriverUnload;
	DriverObject->DriverExtension->AddDevice = AddDevice;

	DriverObject->MajorFunction[IRP_MJ_CREATE] = DispatchCreate;
	DriverObject->MajorFunction[IRP_MJ_CLOSE] = DispatchClose;
	DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = FilterGenericIdleControls;
	DriverObject->MajorFunction[IRP_MJ_CLEANUP] = DispatchCleanup;
	DriverObject->MajorFunction[IRP_MJ_POWER] = DispatchPower;
	DriverObject->MajorFunction[IRP_MJ_PNP] = DispatchPnp;
	
	return STATUS_SUCCESS;
	}							// DriverEntry

///////////////////////////////////////////////////////////////////////////////

#pragma PAGEDCODE

VOID DriverUnload(IN PDRIVER_OBJECT DriverObject)
	{							// DriverUnload
	PAGED_CODE();
	RtlFreeUnicodeString(&servkey);
	}							// DriverUnload

///////////////////////////////////////////////////////////////////////////////

NTSTATUS AddDevice(IN PDRIVER_OBJECT DriverObject, IN PDEVICE_OBJECT pdo)
	{							// AddDevice
	PAGED_CODE();

	NTSTATUS status;

	// Create a functional device object to represent the hardware we're managing.

	PDEVICE_OBJECT fdo;

	ULONG dxsize = (sizeof(DEVICE_EXTENSION) + 7) & ~7;
	ULONG xsize = dxsize + GetSizeofGenericExtension();
	status = IoCreateDevice(DriverObject, xsize, NULL,
		FILE_DEVICE_UNKNOWN, FILE_DEVICE_SECURE_OPEN, FALSE, &fdo);
	if (!NT_SUCCESS(status))
		{						// can't create device object
		KdPrint((DRIVERNAME " - IoCreateDevice failed - %X\n", status));
		return status;
		}						// can't create device object
	
	PDEVICE_EXTENSION pdx = (PDEVICE_EXTENSION) fdo->DeviceExtension;
	BOOLEAN ginit = FALSE;

	// From this point forward, any error will have side effects that need to
	// be cleaned up. Using a try-finally block allows us to modify the program
	// easily without losing track of the side effects.

	__try
		{						// finish initialization
		pdx->DeviceObject = fdo;
		pdx->Pdo = pdo;

		// Link our device object into the stack leading to the PDO
		
		pdx->LowerDeviceObject = IoAttachDeviceToDeviceStack(fdo, pdo);
		if (!pdx->LowerDeviceObject)
			{						// can't attach device
			KdPrint((DRIVERNAME " - IoAttachDeviceToDeviceStack failed\n"));
			status = STATUS_DEVICE_REMOVED;
			__leave;
			}						// can't attach device

		// Set power management flags in the device object

		fdo->Flags |= DO_POWER_PAGABLE;

		// Initialize to use the GENERIC.SYS library

		pdx->pgx = (PGENERIC_EXTENSION) ((PUCHAR) pdx + dxsize);

		INIT_STRUCT gis;
		RtlZeroMemory(&gis, sizeof(gis));
		gis.Size = sizeof(gis);
		gis.DeviceObject = fdo;
		gis.Pdo = pdo;
		gis.Ldo = pdx->LowerDeviceObject;
		gis.RemoveLock = &pdx->RemoveLock;
		gis.StartDevice = StartDevice;
		gis.StopDevice = StopDevice;
		gis.RemoveDevice = RemoveDevice;
		RtlInitUnicodeString(&gis.DebugName, LDRIVERNAME);
		gis.Flags |= GENERIC_IDLE_DETECT;
		gis.Flags |= GENERIC_PENDING_IOCTLS;

		status = InitializeGenericExtension(pdx->pgx, &gis);
		if (!NT_SUCCESS(status))
			{
			KdPrint((DRIVERNAME " - InitializeGenericExtension failed - %X\n", status));
			__leave;
			}
		ginit = TRUE;
		GenericRegisterInterface(pdx->pgx, &GUID_INTERFACE_EVNOTIFY);


		ASSERT(KeGetCurrentIrql() <= DISPATCH_LEVEL);
		KeInitializeTimer(&pdx->timer);
		KeInitializeDpc(&pdx->dpc, (PKDEFERRED_ROUTINE)DpcRoutine, pdx);
		
		// Clear the "initializing" flag so that we can get IRPs

		fdo->Flags &= ~DO_DEVICE_INITIALIZING;
		}						// finish initialization
	__finally
		{						// cleanup side effects
		if (!NT_SUCCESS(status))
			{					// need to cleanup
			if (ginit)
				CleanupGenericExtension(pdx->pgx);
			if (pdx->LowerDeviceObject)
				IoDetachDevice(pdx->LowerDeviceObject);
			IoDeleteDevice(fdo);
			}					// need to cleanup
		}						// cleanup side effects

	return status;
	}							// AddDevice

///////////////////////////////////////////////////////////////////////////////

#pragma LOCKEDCODE

NTSTATUS CompleteRequest(IN PIRP Irp, IN NTSTATUS status, IN ULONG_PTR info)
	{							// CompleteRequest
	Irp->IoStatus.Status = status;
	Irp->IoStatus.Information = info;
	IoCompleteRequest(Irp, IO_NO_INCREMENT);
	return status;
	}							// CompleteRequest

///////////////////////////////////////////////////////////////////////////////

#pragma PAGEDCODE

NTSTATUS DispatchPnp(PDEVICE_OBJECT fdo, PIRP Irp)
	{							// DispatchPnp
	PDEVICE_EXTENSION pdx = (PDEVICE_EXTENSION) fdo->DeviceExtension;
	return GenericDispatchPnp(pdx->pgx, Irp);
	}							// DispatchPnp

NTSTATUS DispatchPower(PDEVICE_OBJECT fdo, PIRP Irp)
	{							// DispatchPower
	PDEVICE_EXTENSION pdx = (PDEVICE_EXTENSION) fdo->DeviceExtension;
	return GenericDispatchPower(pdx->pgx, Irp);
	}							// DispatchPower

NTSTATUS FilterGenericIdleControls(PDEVICE_OBJECT fdo, PIRP Irp)
	{							// FilterGenericIdleControls
	PAGED_CODE();
	PDEVICE_EXTENSION pdx = (PDEVICE_EXTENSION) fdo->DeviceExtension;

	NTSTATUS status = IoAcquireRemoveLock(&pdx->RemoveLock, Irp);
	if (!NT_SUCCESS(status))
		return CompleteRequest(Irp, status, 0);
	ULONG info = 0;

	// Delegate the IRP to GENERIC.SYS

	status = GenericHandlePowerIoctl(pdx->pgx, Irp);
	if (status == STATUS_INVALID_DEVICE_REQUEST)
		status = DispatchControl(fdo, Irp);
	else
		IoCompleteRequest(Irp, IO_NO_INCREMENT);

	IoReleaseRemoveLock(&pdx->RemoveLock, Irp);
	return status;
	}							// FilterGenericIdleControls

///////////////////////////////////////////////////////////////////////////////

#pragma PAGEDCODE

VOID RemoveDevice(IN PDEVICE_OBJECT fdo)
	{							// RemoveDevice
	PAGED_CODE();
	PDEVICE_EXTENSION pdx = (PDEVICE_EXTENSION) fdo->DeviceExtension;
	NTSTATUS status;


	if (pdx->LowerDeviceObject)
		IoDetachDevice(pdx->LowerDeviceObject);

	IoDeleteDevice(fdo);
	}							// RemoveDevice

///////////////////////////////////////////////////////////////////////////////

#if DBG && defined(_X86_)
#pragma LOCKEDCODE

extern "C" void __declspec(naked) __cdecl _chkesp()
	{
	_asm je okay
	ASSERT(!DRIVERNAME " - Stack pointer mismatch!");
okay:
	_asm ret
	}

#endif // DBG
