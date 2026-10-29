#include <availability_domain.h>

int dynamic_domain_pred();

CLANG_ENABLED_AVAILABILITY_DOMAIN(EnabledDomain);
CLANG_DISABLED_AVAILABILITY_DOMAIN(DisabledDomain);
CLANG_DYNAMIC_AVAILABILITY_DOMAIN(DynamicDomain, dynamic_domain_pred);

#define AVAIL 0
#define UNAVAIL 1

#define UNAVAILABLE_ON_APPLE_PLATFORMS                                         \
  __attribute__((availability(macos, unavailable)))                            \
  __attribute__((availability(ios, unavailable)))                              \
  __attribute__((availability(tvos, unavailable)))                             \
  __attribute__((availability(watchos, unavailable)))                          \
  __attribute__((availability(visionos, unavailable)))

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

typedef enum __attribute__((enum_extensibility(closed))) : long {
  ClosedEnumAlwaysAvailable = 0,
  ClosedEnumEnabledDomain
      __attribute__((availability(domain : EnabledDomain, AVAIL))) = 1,
  ClosedEnumDisabledDomain
      __attribute__((availability(domain : DisabledDomain, AVAIL))) = 2,
  ClosedEnumDynamicDomain
      __attribute__((availability(domain : DynamicDomain, AVAIL))) = 3,
  ClosedEnumUnavailableEnabledDomain __attribute__((unavailable))
      __attribute__((availability(domain : EnabledDomain, AVAIL))) = 4,
  ClosedEnumPlatformUnavailableEnabledDomain UNAVAILABLE_ON_APPLE_PLATFORMS
      __attribute__((availability(domain : EnabledDomain, AVAIL))) = 5,
} ClosedEnum;

typedef enum __attribute__((enum_extensibility(open))) : long {
  OpenEnumAlwaysAvailable = 0,
  OpenEnumEnabledDomain
      __attribute__((availability(domain : EnabledDomain, AVAIL))) = 1,
  OpenEnumDisabledDomain
      __attribute__((availability(domain : DisabledDomain, AVAIL))) = 2,
  OpenEnumDynamicDomain
      __attribute__((availability(domain : DynamicDomain, AVAIL))) = 3,
} OpenEnum;
