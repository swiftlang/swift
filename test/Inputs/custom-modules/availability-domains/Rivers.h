#include <feature-availability.h>

static struct __AvailabilityDomain colorado_domain __attribute__((
    availability_domain(Colorado))) = {__AVAILABILITY_DOMAIN_DISABLED, 0};

#define AVAIL 0
#define UNAVAIL 1

__attribute__((availability(domain:Colorado, AVAIL)))
void available_in_colorado(void);

#undef UNAVAIL
#undef AVAIL
