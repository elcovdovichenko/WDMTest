/*++

Copyright (c) 2001  ELCO  All Rights Reserved

Module Name:

    TEST.C

Abstract:

    Lists all the function device and interfaces present in the system
    and opens the last interface to send an invalidate device Ioctl request
    or read requests.

Environment:

    usermode console application

Revision History:

    Vyacheslav Vdovichenko Jan 22 2001

--*/

#include <basetyps.h>
#include <stdlib.h>
#include <wtypes.h>
#include <setupapi.h>
#include <initguid.h>
#include <stdio.h>
#include <string.h>
#include <winioctl.h>
#include "..\inc\public.h"
#include <conio.h>

int _cdecl main (int argc, char *argv[])
{
    HDEVINFO                            hardwareDeviceInfo;
    SP_INTERFACE_DEVICE_DATA            deviceInterfaceData;
    PSP_INTERFACE_DEVICE_DETAIL_DATA    deviceInterfaceDetailData = NULL;
    ULONG                               predictedLength = 0;
    ULONG                               requiredLength = 0;
    HANDLE                              file;
    int                                 i;
    ULONG                               operation = 10;

    //
    // Open a handle to the device interface information set of all 
    // present toaster class interfaces.
    //

    hardwareDeviceInfo = SetupDiGetClassDevs (
                       (LPGUID)&GUID_FUNCTION_DEVICE_INTERFACE_CLASS,
                       NULL, // Define no enumerator (global)
                       NULL, // Define no
                       (DIGCF_PRESENT | // Only Devices present
                       DIGCF_INTERFACEDEVICE)); // Function class devices.

    if(INVALID_HANDLE_VALUE == hardwareDeviceInfo)
    {
        printf("SetupDiGetClassDevs failed: %x\n", GetLastError());
        return 0;
    }

    deviceInterfaceData.cbSize = sizeof (SP_INTERFACE_DEVICE_DATA);

    printf("\nList of Skeleton Function Device Interfaces\n");
    printf("---------------------------------\n");

    i = 0;

    //
    // Enumerate devices of Skeleton class
    //

    do {
        if (SetupDiEnumDeviceInterfaces (hardwareDeviceInfo,
                                 0, // No care about specific PDOs
                                 (LPGUID)&GUID_FUNCTION_DEVICE_INTERFACE_CLASS,
                                 i, //
                                 &deviceInterfaceData)) {

            if(deviceInterfaceDetailData)
                free (deviceInterfaceDetailData);
            //
            // Allocate a function class device data structure to
            // receive the information about this particular device.
            //

            //
            // First find out required length of the buffer
            //

            SetupDiGetInterfaceDeviceDetail (
                    hardwareDeviceInfo,
                    &deviceInterfaceData,
                    NULL, // probing so no output buffer yet
                    0, // probing so output buffer length of zero
                    &requiredLength,
                    NULL); // not interested in the specific dev-node


            predictedLength = requiredLength;

            deviceInterfaceDetailData = malloc (predictedLength);
            deviceInterfaceDetailData->cbSize =
                            sizeof (SP_INTERFACE_DEVICE_DETAIL_DATA);


            if (! SetupDiGetInterfaceDeviceDetail (
                       hardwareDeviceInfo,
                       &deviceInterfaceData,
                       deviceInterfaceDetailData,
                       predictedLength,
                       &requiredLength,
                       NULL)) {
                printf("Error in SetupDiGetInterfaceDeviceDetail\n");
                free (deviceInterfaceDetailData);
                return FALSE;
            }
            printf("%d) %s\n", ++i,
                    deviceInterfaceDetailData->DevicePath);
        }
        else if (ERROR_NO_MORE_ITEMS != GetLastError()) {
            free (deviceInterfaceDetailData);
            deviceInterfaceDetailData = NULL;
            continue;
        }
        else
            break;

    } while (TRUE);


    SetupDiDestroyDeviceInfoList (hardwareDeviceInfo);

    if(!i)
    {
        printf("No device interfaces present\n");
        return 0;
    }

    //
    // Open the last toaster device interface
    //

    printf("\nOpening the last interface:\n %s\n",
                    deviceInterfaceDetailData->DevicePath);

    file = CreateFile ( deviceInterfaceDetailData->DevicePath,
                        GENERIC_READ | GENERIC_WRITE,
                        0,
                        NULL, // no SECURITY_ATTRIBUTES structure
                        OPEN_EXISTING, // No special create flags
                        FILE_ATTRIBUTE_NORMAL,
                        NULL
                        );

    if (INVALID_HANDLE_VALUE != file)
    {
        printf("Wow - it really worked!!!\n");

        // Point proven.  Be a nice program and close up shop.

        if (DeviceIoControl (file,
                             IOCTL_GET_RESULT_OPERATION,
                             &operation, sizeof(operation),
                             &operation, sizeof(operation),
                             &i, NULL)) {
            printf("\nIOCTL Operation = %d Lenght = %d Error:0x%x\n",operation, i, GetLastError());
        }
        else
            printf("Invalidate device request failed:0x%x\n", GetLastError());

        CloseHandle(file);
    }
    else
        printf("Error in CreateFile: %x", GetLastError());

    free (deviceInterfaceDetailData);

    return 0;
}
