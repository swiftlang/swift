@import Foundation;

__attribute__((availability(macosx,introduced=10.0)))
__attribute__((availability(ios,introduced=2.0)))
__attribute__((availability(tvos,introduced=1.0)))
__attribute__((availability(watchos,introduced=2.0)))
__attribute__((availability(maccatalyst,introduced=13.1)))
@interface NSBaseClass : NSObject
- (instancetype) init
  __attribute__((objc_designated_initializer))
  __attribute__((availability(macosx,introduced=10.0)))
  __attribute__((availability(ios,introduced=2.0)))
  __attribute__((availability(tvos,introduced=1.0)))
  __attribute__((availability(watchos,introduced=2.0)))
  __attribute__((availability(maccatalyst,introduced=13.1)));
@end
