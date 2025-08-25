#include <feature-availability.h>

static struct __AvailabilityDomain salt_domain __attribute__((
    availability_domain(Salt))) = {__AVAILABILITY_DOMAIN_ENABLED, 0};

#define AVAIL 0
#define UNAVAIL 1

__attribute__((availability(domain:Salt, AVAIL)))
void available_in_salt(void);

#undef UNAVAIL
#undef AVAIL
