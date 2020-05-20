@import Foundation;

#define SWIFT_NAME(X) __attribute__((swift_name(#X)))

#pragma clang assume_nonnull begin
@interface SwiftNameTest : NSObject
+ (instancetype)g:(id)x outParam:(int *)foo SWIFT_NAME(init(g:));
@end
#pragma clang assume_nonnull end
