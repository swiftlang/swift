#include "rdar85526916.h"

#pragma clang assume_nonnull begin

@implementation PFXObject
- (void)performGetStringIdentityWithCompletionHandler:
    (void (^)(NSString * _Nonnull(^ _Nonnull)(NSString * _Nonnull)))completionHandler {
  completionHandler(^(NSString * _Nonnull input) {
    return input;
  });
}
- (void)performGetStringAppendWithCompletionHandler:
    (void (^)(NSString * _Nonnull(^ _Nonnull)(NSString * _Nonnull, NSString * _Nonnull)))completionHandler {
  completionHandler(^(NSString * _Nonnull one, NSString * _Nonnull two) {
    return [one stringByAppendingString: two];
  });
}
- (void)performGetIntegerIdentityWithCompletionHandler:
    (void (^)(NSInteger(^ _Nonnull)(NSInteger)))completionHandler {
  completionHandler(^(NSInteger input) {
    return input;
  });
}
- (void)performGetIntegerSubtractWithCompletionHandler:
    (void (^)(NSInteger(^ _Nonnull)(NSInteger, NSInteger)))completionHandler {
  completionHandler(^(NSInteger lhs, NSInteger rhs) {
    return lhs - rhs;
  });
}
- (void)performGetUIntegerIdentityWithCompletionHandler:
    (void (^)(NSUInteger(^ _Nonnull)(NSUInteger)))completionHandler {
  completionHandler(^(NSUInteger input) {
    return input;
  });
}
- (void)performGetUIntegerAddWithCompletionHandler:
    (void (^)(NSUInteger(^ _Nonnull)(NSUInteger, NSUInteger)))completionHandler {
  completionHandler(^(NSUInteger lhs, NSUInteger rhs) {
    return lhs + rhs;
  });
}
@end

#pragma clang assume_nonnull end

