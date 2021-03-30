@import Foundation;

#pragma clang assume_nonnull begin

#define SWIFT_MAIN_ACTOR __attribute__((swift_attr("@MainActor")))
#define SWIFT_UI_ACTOR __attribute__((swift_attr("@UIActor")))

// NOTE: If you ever end up removing support for the "@UIActor" alias,
// just change both to be @MainActor and it won't change the purpose of
// this test.

SWIFT_UI_ACTOR SWIFT_MAIN_ACTOR @protocol DoubleMainActor
@required
- (NSString *)createSeaShanty:(NSInteger)number;
@end

#pragma clang assume_nonnull end
