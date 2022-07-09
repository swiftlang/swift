#include <Foundation/Foundation.h>

#pragma clang assume_nonnull begin

@interface PFXObject : NSObject {
}
- (void)continuePassSyncWithCompletionHandler:(void (^)(void (^_Nullable)(void),
                                                        NSError *_Nullable,
                                                        BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 3)));
- (void)continuePassAsyncWithCompletionHandler:
    (void (^)(void (^_Nullable)(void), NSError *_Nullable,
              BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 3)));
- (void)continueFailSyncWithCompletionHandler:(void (^)(void (^_Nullable)(void),
                                                        NSError *_Nullable,
                                                        BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 3)));
- (void)continueFailAsyncWithCompletionHandler:
    (void (^)(void (^_Nullable)(void), NSError *_Nullable,
              BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 3)));
- (void)continueIncorrectWithCompletionHandler:
    (void (^)(void (^_Nullable)(void), NSError *_Nullable,
              BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 3)));
@end

#pragma clang assume_nonnull end
