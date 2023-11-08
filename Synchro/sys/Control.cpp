// Control.cpp -- IOCTL handlers for sinchro driver
// Copyright (C) 1999, 2000 by Walter Oney
// All rights reserved

#include "stddcls.h"
#include "driver.h"
#include "ioctls.h"

///////////////////////////////////////////////////////////////////////////////

#pragma PAGEDCODE


NTSTATUS DelayThread(LONG delay)
	{							// DelayThread
	
	KdPrint((DRIVERNAME " - DelayThread during %d\n", delay));

	ASSERT(KeGetCurrentIrql() == PASSIVE_LEVEL);
	LARGE_INTEGER duetime = RtlConvertLongToLargeInteger(delay);	
	
	return KeDelayExecutionThread(KernelMode, FALSE, &duetime);
	}							// DelayThread

///////////////////////////////////////////////////////////////////////////////

#pragma PAGEDCODE

NTSTATUS WaitingThread(PDEVICE_EXTENSION pdx, LONG duration)
	{							// WaitingThread
	
	NTSTATUS status = STATUS_SUCCESS;

	KdPrint((DRIVERNAME " - WaitingThread duration is %d\n", duration));

	ASSERT(KeGetCurrentIrql() <= DISPATCH_LEVEL);
	KeInitializeTimerEx(&pdx->wait, SynchronizationTimer);
	
	LARGE_INTEGER duetime = RtlConvertLongToLargeInteger(duration);
	KeSetTimer(&pdx->wait, duetime, NULL);

	status = KeWaitForSingleObject(&pdx->wait, Executive, KernelMode, FALSE, NULL);
	
	KeCancelTimer(&pdx->wait);

	return status;		
	}							// WaitingThread

///////////////////////////////////////////////////////////////////////////////

#pragma PAGEDCODE

NTSTATUS DispatchControl(PDEVICE_OBJECT fdo, PIRP Irp)
	{							// DispatchControl
	PAGED_CODE();
	PDEVICE_EXTENSION pdx = (PDEVICE_EXTENSION) fdo->DeviceExtension;

	NTSTATUS status = IoAcquireRemoveLock(&pdx->RemoveLock, Irp);
	if (!NT_SUCCESS(status))
		return CompleteRequest(Irp, status, 0);
	ULONG info = 0;

	PIO_STACK_LOCATION stack = IoGetCurrentIrpStackLocation(Irp);
	ULONG cbin = stack->Parameters.DeviceIoControl.InputBufferLength;
	ULONG cbout = stack->Parameters.DeviceIoControl.OutputBufferLength;
	ULONG code = stack->Parameters.DeviceIoControl.IoControlCode;

	switch (code)
		{						// process request

	case IOCTL_WAIT_TIMER:				// code == 0x800
		{						// IOCTL_WAIT_TIMER

		KdPrint((DRIVERNAME " - IOCTL_WAIT_TIMER = 0x%x\n", code));
		
		// TODO insert code here to handle this IOCTL, which uses METHOD_BUFFERED
		
		if (cbin != 4)
			{
			KdPrint((DRIVERNAME " - Length control transaction %d is wrong\n", cbin));
			status = STATUS_INVALID_PARAMETER;
			break;
			}
		
		PLONG pw = (PLONG)Irp->AssociatedIrp.SystemBuffer;
		LONG duration = *pw;

		NTSTATUS status = WaitingThread(pdx, duration);

		if (NT_SUCCESS(status))
		{
		switch (status)
			{
		case STATUS_SUCCESS:
			{
			KdPrint((DRIVERNAME " - STATUS_SUCCESS\n"));
			*pw = 1;
			break;
			}

		case STATUS_TIMEOUT:
			{
			KdPrint((DRIVERNAME " - STATUS_TIMEOUT\n"));
			*pw = 2;
			break;
			}

		default:
			KdPrint((DRIVERNAME " - Status SUCCESS\n"));
			*pw = 0;
			break;
			}
		}
		else
		{
			KdPrint((DRIVERNAME " - Status NOT SUCCESS\n"));
			*pw = -1;
		}
		
		info = 4;
			
		break;
		}						// IOCTL_WAIT_TIMER

	case IOCTL_NOTIFY_EVENT:				// code == 0x801
		{						// IOCTL_NOTIFY_EVENT

		//KdPrint((DRIVERNAME " - IOCTL_NOTIFY_EVENT = 0x%x\n", code));

		// TODO insert code here to handle this IOCTL, which uses METHOD_BUFFERED
		
		if (cbout != 4)
			{
			KdPrint((DRIVERNAME " - Length control transaction %d is wrong\n", cbin));
			status = STATUS_INVALID_PARAMETER;
			break;
			}
		
		PLONG pw = (PLONG)Irp->AssociatedIrp.SystemBuffer;

		*pw = pdx->counter;
		
		info = 4;

		break;
		}						// IOCTL_NOTIFY_EVENT

	case IOCTL_DELAY_THREAD:				// code == 0x802
		{						// IOCTL_DELAY_THREAD

		KdPrint((DRIVERNAME " - IOCTL_DELAY_THREAD = 0x%x\n", code));
		
		// TODO insert code here to handle this IOCTL, which uses METHOD_BUFFERED
		
		if (cbin != 4)
			{
			KdPrint((DRIVERNAME " - Length control transaction %d is wrong\n", cbin));
			status = STATUS_INVALID_PARAMETER;
			break;
			}
		
		PLONG pw = (PLONG)Irp->AssociatedIrp.SystemBuffer;
		LONG duration = *pw;

		NTSTATUS status = DelayThread(duration);

		if (NT_SUCCESS(status))
		{
		switch (status)
			{
		case STATUS_SUCCESS:
			{
			KdPrint((DRIVERNAME " - STATUS_SUCCESS\n"));
			*pw = 1;
			break;
			}

		case STATUS_TIMEOUT:
			{
			KdPrint((DRIVERNAME " - STATUS_TIMEOUT\n"));
			*pw = 2;
			break;
			}

		default:
			KdPrint((DRIVERNAME " - Status SUCCESS\n"));
			*pw = 0;
			break;
			}
		}
		else
		{
			KdPrint((DRIVERNAME " - Status NOT SUCCESS\n"));
			*pw = -1;
		}
		
		info = 4;
						
		break;
		}						// IOCTL_DELAY_THREAD

	default:
		status = STATUS_INVALID_DEVICE_REQUEST;
		break;

		}						// process request

	IoReleaseRemoveLock(&pdx->RemoveLock, Irp);
	return CompleteRequest(Irp, status, info);
	}							// DispatchControl
