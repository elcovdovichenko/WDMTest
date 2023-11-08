/*++

Copyright (c) 2001  ELCO  All Rights Reserved

Module Name:

    public.h

Abstract:

    This module contains the common declarations shared by driver
    and user applications.

Environment:

    user and kernel

Revision History:

    Vyacheslav Vdovichenko Jan 12 2001

Notes:

--*/

//
// Define an Interface Guid for function device class.
// This GUID is used to register (IoRegisterDeviceInterface)
// an instance of an interface so that user application
// can control the function device.
//

DEFINE_GUID (GUID_FUNCTION_DEVICE_INTERFACE_CLASS,
   0x8eeb40e7, 0x7726, 0x4cd6, 0x92, 0x24, 0x80, 0xde, 0x12, 0xc0, 0x91, 0x1);
// {8EEB40E7-7726-4cd6-9224-80DE12C09101}

//
// Define a Setup Class GUID for Skeleton Class. This is same
// as the SKELETON CLASS guid in the INF files.
//

DEFINE_GUID (GUID_SKELETON_SETUP_CLASS,
   0x32B04CA9, 0x6DE2, 0x47af, 0x9F, 0xEE, 0x20, 0x6D, 0x1F, 0x2B, 0xF6, 0x29);
// {32B04CA9-6DE2-47af-9FEE-206D1F2BF629}

//
// These are the states Function transition to upon
// receiving a specific PnP Irp. Refer to the PnP Device States
// diagram in DDK documentation for better understanding.
//

#ifndef __PUBLIC_H
#define __PUBLIC_H

//#define IOCTL_BUSENUM_PLUGIN_HARDWARE               BUSENUM_IOCTL (0x0)
//#define IOCTL_BUSENUM_UNPLUG_HARDWARE               BUSENUM_IOCTL (0x1)
//#define IOCTL_BUSENUM_EJECT_HARDWARE                BUSENUM_IOCTL (0x2)
//#define IOCTL_TOASTER_DONT_DISPLAY_IN_UI_DEVICE     BUSENUM_IOCTL (0x3)

//
//  Data structure used in PlugIn and UnPlug ioctls
//

#endif


