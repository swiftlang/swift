#include <Rivers.h>
#include <feature-availability.h>

int arctic_pred(void);
int pacific_pred(void);

static struct __AvailabilityDomain __Arctic
    __attribute__((availability_domain(Arctic))) = {
        __AVAILABILITY_DOMAIN_DYNAMIC, arctic_pred};
static struct __AvailabilityDomain __Pacific
    __attribute__((availability_domain(Pacific))) = {
        __AVAILABILITY_DOMAIN_DYNAMIC, pacific_pred};

#define AVAIL 0
#define UNAVAIL 1

__attribute__((availability(domain:Arctic, AVAIL)))
void available_in_arctic(void);

__attribute__((availability(domain:Pacific, UNAVAIL)))
void unavailable_in_pacific(void);

__attribute__((availability(domain:Colorado, AVAIL)))
__attribute__((availability(domain:Pacific, AVAIL)))
void available_in_colorado_river_delta(void);

#undef UNAVAIL
#undef AVAIL
