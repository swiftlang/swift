#include <feature-availability.h>

static struct __AvailabilityDomain __EnabledDomain
    __attribute__((availability_domain(EnabledDomain))) = {
        __AVAILABILITY_DOMAIN_ENABLED, 0};

static struct __AvailabilityDomain __DisabledDomain
    __attribute__((availability_domain(DisabledDomain))) = {
        __AVAILABILITY_DOMAIN_DISABLED, 0};

int dynamic_domain_pred();

static struct __AvailabilityDomain __DynamicDomain
    __attribute__((availability_domain(DynamicDomain))) = {
        __AVAILABILITY_DOMAIN_DYNAMIC, dynamic_domain_pred};

#define AVAIL 0
#define UNAVAIL 1

__attribute__((availability(domain:EnabledDomain, AVAIL)))
void available_in_enabled_domain(void);

__attribute__((availability(domain:EnabledDomain, UNAVAIL)))
void unavailable_in_enabled_domain(void);

__attribute__((availability(domain:DisabledDomain, AVAIL)))
void available_in_disabled_domain(void);

__attribute__((availability(domain:DisabledDomain, UNAVAIL)))
void unavailable_in_disabled_domain(void);

__attribute__((availability(domain:DynamicDomain, AVAIL)))
void available_in_dynamic_domain(void);

__attribute__((availability(domain:DynamicDomain, UNAVAIL)))
void unavailable_in_dynamic_domain(void);
