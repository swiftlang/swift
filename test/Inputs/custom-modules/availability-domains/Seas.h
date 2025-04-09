#include <feature-availability.h>

static struct __AvailabilityDomain __Baltic __attribute__((
    availability_domain(Baltic))) = {__AVAILABILITY_DOMAIN_ENABLED, 0};
static struct __AvailabilityDomain __Mediterranean __attribute__((
    availability_domain(Mediterranean))) = {__AVAILABILITY_DOMAIN_ENABLED, 0};

#define AVAIL 0
#define UNAVAIL 1

__attribute__((availability(domain:Baltic, AVAIL)))
void available_in_baltic(void);

#undef UNAVAIL
#undef AVAIL
