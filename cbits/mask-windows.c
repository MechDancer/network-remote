#include <winsock2.h>
#include <iphlpapi.h>
#include <stdio.h>
#include <stdlib.h>

#pragma comment(lib, "IPHLPAPI.lib")

#define MALLOC(x) HeapAlloc(GetProcessHeap(), 0, (x))
#define FREE(x) HeapFree(GetProcessHeap(), 0, (x))

void foo() {

    PIP_ADAPTER_INFO pAdapterInfo;
    ULONG ulOutBufLen = sizeof(IP_ADAPTER_INFO);
    pAdapterInfo = (IP_ADAPTER_INFO *) MALLOC(sizeof(IP_ADAPTER_INFO));
    GetAdaptersInfo(pAdapterInfo, &ulOutBufLen);
    printf("\tIP Mask: \t%s\n", pAdapterInfo->IpAddressList.IpMask.String);
    if (pAdapterInfo)
        FREE(pAdapterInfo);

}

char *c_get_subnet_mask(const char *interface_name) {
    // TODO break foo()
    return NULL;
}