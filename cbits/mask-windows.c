#include <WinSock2.h>
#include <iphlpapi.h>
#include <stdio.h>
#include <string.h>

#pragma comment(lib, "IPHLPAPI.lib")

#define MALLOC(x) HeapAlloc(GetProcessHeap(), 0, (x))
#define FREE(x) HeapFree(GetProcessHeap(), 0, (x))

char *c_get_subnet_mask(const char *interface_ip_address)
{
    {

        PIP_ADAPTER_INFO pAdapterInfo;
        PIP_ADAPTER_INFO pAdapter = NULL;

        char *target = NULL;

        ULONG ulOutBufLen = sizeof(IP_ADAPTER_INFO);
        pAdapterInfo = (IP_ADAPTER_INFO *)MALLOC(sizeof(IP_ADAPTER_INFO));
        if (pAdapterInfo == NULL)
        {
            printf("Error allocating memory needed to call GetAdaptersinfo\n");
            return NULL;
        }
        if (GetAdaptersInfo(pAdapterInfo, &ulOutBufLen) == ERROR_BUFFER_OVERFLOW)
        {
            FREE(pAdapterInfo);
            pAdapterInfo = (IP_ADAPTER_INFO *)MALLOC(ulOutBufLen);
            if (pAdapterInfo == NULL)
            {
                return NULL;
            }
        }

        if ((GetAdaptersInfo(pAdapterInfo, &ulOutBufLen)) == NO_ERROR)
        {
            pAdapter = pAdapterInfo;
            while (pAdapter)
            {
                target = pAdapter->IpAddressList.IpMask.String;
                if (strcmp(interface_ip_address, pAdapter->IpAddressList.IpAddress.String) == 0)
                    break;
                pAdapter = pAdapter->Next;
            }
        }
        FREE(pAdapterInfo);

        return target;
    }
}
