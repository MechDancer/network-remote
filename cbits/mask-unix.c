#include <arpa/inet.h>
#include <sys/socket.h>
#include <ifaddrs.h>
#include <string.h>

char *c_get_subnet_mask(const char *interface_ip_address) {
    struct ifaddrs *ifap, *ifa;
    struct sockaddr_in *sa, *addr;
    char *mask = NULL;

    getifaddrs(&ifap);
    for (ifa = ifap; ifa; ifa = ifa->ifa_next) {
        if (ifa->ifa_addr->sa_family == AF_INET) {
            sa = (struct sockaddr_in *) ifa->ifa_netmask;
            addr = (struct sockaddr_in *) ifa->ifa_addr;
            if (strcmp(interface_ip_address, inet_ntoa(addr->sin_addr)) == 0) {
                mask = inet_ntoa(sa->sin_addr);
                break;
            }
        }
    }
    freeifaddrs(ifap);
    return mask;
}