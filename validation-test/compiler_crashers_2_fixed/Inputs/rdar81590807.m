#include "rdar81590807.h"

#pragma clang assume_nonnull begin

@implementation PFXObject
- (void)continuePassSyncWithCompletionHandler:(void (^)(void (^_Nullable)(void),
                                                        NSError *_Nullable,
                                                        BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 3))) {
  completionHandler(
      ^{
        NSLog(@"passSync");
      },
      NULL, YES);
}
- (void)continuePassAsyncWithCompletionHandler:
    (void (^)(void (^_Nullable)(void), NSError *_Nullable,
              BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 3))) {
  dispatch_async(dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0), ^{
    completionHandler(
        ^{
          NSLog(@"passAsync");
        },
        NULL, YES);
  });
}
- (void)continueFailSyncWithCompletionHandler:(void (^)(void (^_Nullable)(void),
                                                        NSError *_Nullable,
                                                        BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 3))) {
  completionHandler(
      NULL, [NSError errorWithDomain:@"failSync" code:1 userInfo:nil], NO);
}
- (void)continueFailAsyncWithCompletionHandler:
    (void (^)(void (^_Nullable)(void), NSError *_Nullable,
              BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 3))) {
  dispatch_async(dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0), ^{
    completionHandler(
        NULL, [NSError errorWithDomain:@"failAsync" code:2 userInfo:nil], NO);
  });
}
- (void)continueIncorrectWithCompletionHandler:
    (void (^)(void (^_Nullable)(void), NSError *_Nullable,
              BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 3))) {
  dispatch_async(dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0), ^{
    completionHandler(NULL, NULL, NO);
  });
}
@end

#pragma clang assume_nonnull end
