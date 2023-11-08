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
#include <winbase.h>
#include "..\inc\public.h"
#include <conio.h>

int _cdecl main (int argc, char *argv[])
{
    HANDLE                              file;
    ULONG                               operation = 10;
    int                                 i;

    //
    // Open a handle to the device
    //

    OutputDebugString("\nOpening the device FDSECOND\n");

    printf("\nOpening the device FDSECOND");

    file = CreateFile ( "\\\\.\\FDSECOND",
                        GENERIC_READ | GENERIC_WRITE,
                        0,
                        NULL,
                        OPEN_EXISTING,
                        0,
                        NULL);

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

        OutputDebugString("Closeing the device FDSECOND\n");

        CloseHandle(file);
    }
    else
        printf("Error in CreateFile: %x", GetLastError());

    return 0;
}
