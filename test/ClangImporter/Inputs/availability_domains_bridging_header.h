#include <feature-availability.h>

static struct __AvailabilityDomain __BayBridge
    __attribute__((availability_domain(BayBridge))) = {
        __AVAILABILITY_DOMAIN_ENABLED, 0};
static struct __AvailabilityDomain __GoldenGateBridge
    __attribute__((availability_domain(GoldenGateBridge))) = {
        __AVAILABILITY_DOMAIN_DISABLED, 0};
