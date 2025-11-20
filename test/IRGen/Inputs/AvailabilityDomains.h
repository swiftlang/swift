#include <availability_domain.h>

int dynamic_domain_pred();

CLANG_ENABLED_AVAILABILITY_DOMAIN(EnabledDomain);
CLANG_DISABLED_AVAILABILITY_DOMAIN(DisabledDomain);
CLANG_DYNAMIC_AVAILABILITY_DOMAIN(DynamicDomain, dynamic_domain_pred);

#define AVAIL 0
#define UNAVAIL 1

__attribute__((availability(domain : EnabledDomain, AVAIL))) void
available_in_enabled_domain(void);

__attribute__((availability(domain : EnabledDomain, UNAVAIL))) void
unavailable_in_enabled_domain(void);

__attribute__((availability(domain : DisabledDomain, AVAIL))) void
available_in_disabled_domain(void);

__attribute__((availability(domain : DisabledDomain, UNAVAIL))) void
unavailable_in_disabled_domain(void);

__attribute__((availability(domain : DynamicDomain, AVAIL))) void
available_in_dynamic_domain(void);

__attribute__((availability(domain : DynamicDomain, UNAVAIL))) void
unavailable_in_dynamic_domain(void);
