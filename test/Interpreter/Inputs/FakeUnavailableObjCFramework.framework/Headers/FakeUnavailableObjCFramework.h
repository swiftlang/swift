#include <Foundation/NSObject.h>

extern int UnavailableObjCGlobalVariable __attribute__((availability(macosx,introduced=1066.0)))  __attribute__((availability(ios,introduced=1066.0)))  __attribute__((availability(visionos,introduced=1066.0)));

__attribute__((availability(macosx,introduced=1066.0)))  __attribute__((availability(ios,introduced=1066.0)))  __attribute__((availability(visionos,introduced=1066.0)))
@protocol UnavailableObjCProtocol
- (void)someMethod;
@end

// This protocol is intentionally missing availability annotations.
@protocol UnannotatedUnavailableObjCProtocol
- (void)someMethod;
@end

__attribute__((availability(macosx,introduced=1066.0)))  __attribute__((availability(ios,introduced=1066.0)))  __attribute__((availability(visionos,introduced=1066.0)))
@interface UnavailableObjCClass : NSObject
- (void)someMethod;
@end

__attribute__((availability(macosx,introduced=1066.0)))  __attribute__((availability(ios,introduced=1066.0)))__attribute__((availability(visionos,introduced=1066.0)))
void someFunction();
