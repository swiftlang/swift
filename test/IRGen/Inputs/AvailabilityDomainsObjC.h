#include "AvailabilityDomains.h"

@import Foundation;

#define AVAILABLE_IN_DOMAIN(dom) __attribute__((availability(domain: dom, AVAIL)))

@interface AlwaysAvailableObjCImplementationClass: NSObject
- (void)enabledDomainMethod AVAILABLE_IN_DOMAIN(EnabledDomain);
- (void)dynamicDomainMethod AVAILABLE_IN_DOMAIN(DynamicDomain);
- (void)disabledDomainMethod AVAILABLE_IN_DOMAIN(DisabledDomain);

@end

AVAILABLE_IN_DOMAIN(EnabledDomain)
@interface EnabledDomainObjCImplementationClass: NSObject
@end

AVAILABLE_IN_DOMAIN(DynamicDomain)
@interface DynamicDomainObjCImplementationClass: NSObject
@end

AVAILABLE_IN_DOMAIN(DisabledDomain)
@interface DisabledDomainObjCImplementationClass: NSObject
@end
