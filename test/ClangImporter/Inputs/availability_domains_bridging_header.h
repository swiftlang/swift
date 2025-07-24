#include <feature-availability.h>

static struct __AvailabilityDomain bay_bridge
    __attribute__((availability_domain(BayBridge))) = {
        __AVAILABILITY_DOMAIN_ENABLED, 0};
static struct __AvailabilityDomain golden_gate_bridge
    __attribute__((availability_domain(GoldenGateBridge))) = {
        __AVAILABILITY_DOMAIN_DISABLED, 0};

#define AVAIL 0
#define UNAVAIL 1


#if __OBJC__
@import Foundation;

__attribute__((availability(domain:BayBridge, AVAIL)))
@interface BayBridgeAvailable : NSObject
- (instancetype)init;
@end

__attribute__((availability(domain:BayBridge, UNAVAIL)))
@interface BayBridgeUnavailable : NSObject
- (instancetype)init;
@end

@interface ImplementMe : NSObject
- (instancetype)init;
- (void)availableInBayBridge __attribute__((availability(domain:BayBridge, AVAIL)));
- (void)unavailableInBayBridge __attribute__((availability(domain:BayBridge, UNAVAIL)));

- (void)availableInGoldenGateBridge __attribute__((availability(domain:GoldenGateBridge, AVAIL)));
- (void)unavailableInGoldenGateBridge __attribute__((availability(domain:GoldenGateBridge, UNAVAIL)));

@end

__attribute__((availability(domain:BayBridge, AVAIL)))
@interface ImplementMeBayBridgeAvailable : NSObject
- (instancetype)init;
@end

__attribute__((availability(domain:BayBridge, UNAVAIL)))
@interface ImplementMeBayBridgeUnavailable : NSObject
- (instancetype)init;
@end

__attribute__((availability(domain:GoldenGateBridge, AVAIL)))
@interface ImplementMeGoldenGateBridgeAvailable : NSObject
- (instancetype)init;
@end

__attribute__((availability(domain:GoldenGateBridge, AVAIL)))
@interface ImplementMeGoldenGateBridgeAvailable2 : NSObject
- (instancetype)init;
@end

__attribute__((availability(domain:GoldenGateBridge, AVAIL)))
@interface ImplementMeGoldenGateBridgeAvailable3 : NSObject
- (instancetype)init;
@end

__attribute__((availability(domain:GoldenGateBridge, UNAVAIL)))
@interface ImplementMeGoldenGateBridgeUnavailable : NSObject
- (instancetype)init;
@end

#endif // __OBJC__

#undef UNAVAIL
#undef AVAIL
