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

    Vyacheslav Vdovichenko Jan 22 2001

Notes:

--*/

//
// Define an Interface Guid for function device class.
// This GUID is used to register (IoRegisterDeviceInterface)
// an instance of an interface so that user application
// can control the function device.
//

DEFINE_GUID (GUID_FUNCTION_DEVICE_INTERFACE_CLASS,
   0x2fed2fe8, 0x6312, 0x4e24, 0xb9, 0x53, 0xda, 0x1, 0x37, 0x6a, 0xed, 0x5b);
// {2FED2FE8-6312-4e24-B953-DA01376AED5B}

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


#define BUSENUM_IOCTL(_index_) \
    CTL_CODE (FILE_DEVICE_UNKNOWN, _index_, METHOD_BUFFERED, FILE_ANY_ACCESS)

#define IOCTL_GET_RESULT_OPERATION                      BUSENUM_IOCTL (0x800)

//
//  Data structure used in PlugIn and UnPlug ioctls
//

#endif


