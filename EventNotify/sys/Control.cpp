// Control.cpp -- IOCTL handlers for evnotify driver
// Copyright (C) 1999, 2000 by Walter Oney
// All rights reserved

#include "stddcls.h"
#include "driver.h"
#include "ioctls.h"

///////////////////////////////////////////////////////////////////////////////

#pragma PAGEDCODE

NTSTATUS DispatchControl(PDEVICE_OBJECT fdo, PIRP Irp)
	{							// DispatchControl

	KdPrint((DRIVERNAME " - DispatchControl Entry\n"));

	PDEVICE_EXTENSION pdx = (PDEVICE_EXTENSION) fdo->DeviceExtension;

	NTSTATUS status = IoAcquireRemoveLock(&pdx->RemoveLock, Irp);
	if (!NT_SUCCESS(status))
		return CompleteRequest(Irp, status, 0);
	ULONG info = 0;

	PIO_STACK_LOCATION stack = IoGetCurrentIrpStackLocation(Irp);
	ULONG cbin = stack->Parameters.DeviceIoControl.InputBufferLength;
	ULONG cbout = stack->Parameters.DeviceIoControl.OutputBufferLength;
	ULONG code = stack->Parameters.DeviceIoControl.IoControlCode;
		
	KdPrint((DRIVERNAME " - DispatchControl Code = 0x%x\n", code));
	
	switch (code)
		{						// process request

	case IOCTL_MANUAL_ACTIVITY:				// code == 0x800
		{						// IOCTL_MANUAL_ACTIVITY

		KdPrint((DRIVERNAME " - IOCTL_MANUAL_ACTIVITY = 0x%x\n", code));


		// TODO insert code here to handle this IOCTL, which uses METHOD_BUFFERED
		if (cbin < sizeof(ULONG))
		{					// buffer too small
			status = STATUS_INVALID_PARAMETER;
			KdPrint((DRIVERNAME " - Length transaction %d is wrong\n", cbin));
			break;
		}					// buffer too small

		PIRP nfyirp = GenericUncacheControlRequest(pdx->pgx, &pdx->ManualIrp);
		if (nfyirp)
		{					// complete notification IRP
			KdPrint((DRIVERNAME " - Event IRP complete\n"));
			*(PULONG) nfyirp->AssociatedIrp.SystemBuffer = *(PULONG) Irp->AssociatedIrp.SystemBuffer;
			CompleteRequest(nfyirp, STATUS_SUCCESS, sizeof(ULONG));
		}					// complete notification IRP
		else
		{
			KdPrint((DRIVERNAME " - status = STATUS_UNSUCCESSFUL\n"));
			status = STATUS_UNSUCCESSFUL;
		}

		break;
		}						// IOCTL_MANUAL_ACTIVITY

	case IOCTL_TIMER_ACTIVITY:				// code == 0x801
		{						// IOCTL_TIMER_ACTIVITY


		KdPrint((DRIVERNAME " - IOCTL_TIMER_ACTIVITY = 0x%x\n", code));

		// TODO insert code here to handle this IOCTL, which uses METHOD_BUFFERED
		if (cbin < sizeof(ULONG))
		{					// buffer too small
			status = STATUS_INVALID_PARAMETER;
			KdPrint((DRIVERNAME " - Length transaction %d is wrong\n", cbin));
			break;
		}					// buffer too small

		pdx->counter = 0;
		LARGE_INTEGER duetime = RtlConvertLongToLargeInteger(0);
		ULONG interval = *(PULONG) Irp->AssociatedIrp.SystemBuffer;
		KeSetTimerEx(&pdx->timer, duetime, interval, &pdx->dpc);

		break;
		}						// IOCTL_TIMER_ACTIVITY

	case IOCTL_MANUAL_EVENT:				// code == 0x802
		{						// IOCTL_MANUAL_EVENT

		KdPrint((DRIVERNAME " - IOCTL_MANUAL_EVENT = 0x%x\n", code));

		// TODO This is an asynchronous IOCTL using METHOD_BUFFERED. You should have
		// a PIRP member of the device extension reserved to point to the currently
		// outstanding IRP of this type. Follow this template for handling this
		// operation:
		//

		if (cbout < sizeof(ULONG))
		{
			KdPrint((DRIVERNAME " - Length transaction %d is wrong\n", cbout));
			status = STATUS_INVALID_PARAMETER;
		}
		else
		{
			status = GenericCacheControlRequest(pdx->pgx, Irp, &pdx->ManualIrp);
			if (NT_SUCCESS(status))
				KdPrint((DRIVERNAME " - IOCTL_MANUAL_EVENT is successful\n"));
			else
				KdPrint((DRIVERNAME " - IOCTL_MANUAL_EVENT error"));
		}
		
		break;
		}						// IOCTL_MANUAL_EVENT

	case IOCTL_TIMER_EVENT:				// code == 0x803
		{						// IOCTL_TIMER_EVENT

		KdPrint((DRIVERNAME " - IOCTL_TIMER_EVENT = 0x%x\n", code));

		// TODO This is an asynchronous IOCTL using METHOD_BUFFERED. You should have
		// a PIRP member of the device extension reserved to point to the currently
		// outstanding IRP of this type. Follow this template for handling this
		// operation:

		if (cbout < sizeof(ULONG))
		{
			KdPrint((DRIVERNAME " - Length transaction %d is wrong\n", cbout));
			status = STATUS_INVALID_PARAMETER;
		}
		else
		{
			status = GenericCacheControlRequest(pdx->pgx, Irp, &pdx->TimerIrp);
			if (NT_SUCCESS(status))
				KdPrint((DRIVERNAME " - IOCTL_TIMER_EVENT is successful\n"));
			else
				KdPrint((DRIVERNAME " - IOCTL_TIMER_EVENT error"));
		}
		
		break;
		}						// IOCTL_TIMER_EVENT

	default:
		status = STATUS_INVALID_DEVICE_REQUEST;
		break;

		}						// process request

	IoReleaseRemoveLock(&pdx->RemoveLock, Irp);
	return status == STATUS_PENDING ? status : CompleteRequest(Irp, status, info);
	}							// DispatchControl


///////////////////////////////////////////////////////////////////////////////

#pragma LOCKEDCODE

VOID DpcRoutine(PKDPC dpc, PDEVICE_EXTENSION pdx, PVOID junk1, PVOID junk2)
	{							// DpcRoutine
		KdPrint((DRIVERNAME " - DpcRoutine\n"));

		if (!(pdx->TimerIrp))
			return;

		PIRP nfyirp = GenericUncacheControlRequest(pdx->pgx, &pdx->TimerIrp);
		if (nfyirp)
		{					// complete notification IRP
			KdPrint((DRIVERNAME " - Timer IRP complete = STATUS_SUCCESS\n"));
			*(PLONG) nfyirp->AssociatedIrp.SystemBuffer = pdx->counter++;
			CompleteRequest(nfyirp, STATUS_SUCCESS, sizeof(LONG));
		}					// complete notification IRP
		else
		{
			KdPrint((DRIVERNAME " - Timer IRP complete = STATUS_UNSUCCESSFUL\n"));
		}
	}							// DpcRoutine

