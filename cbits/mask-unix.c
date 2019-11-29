#include <arpa/inet.h>
#include <sys/socket.h>
#include <ifaddrs.h>
#include <stdio.h>
#include <string.h>

char *c_get_subnet_mask(const char *interface_ip_address) {
    struct ifaddrs *ifap, *ifa;
    struct sockaddr_in *sa;
    char *addr = NULL;

    getifaddrs(&ifap);
    for (ifa = ifap; ifa; ifa = ifa->ifa_next) {
        if (ifa->ifa_addr->sa_family == AF_INET) {
            sa = (struct sockaddr_in *) ifa->ifa_netmask;
            addr = inet_ntoa(sa->sin_addr);
            if (strcmp(interface_ip_address, ifa->ifa_addr->ifa_name) == 0)
                break;
        }
    }
    freeifaddrs(ifap);
    return addr;
}