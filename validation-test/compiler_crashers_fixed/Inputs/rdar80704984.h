#include <Foundation/Foundation.h>

#pragma clang assume_nonnull begin

typedef void (^CompletionHandler)(int status, NSUInteger bytesTransferred);

@interface PFXObject : NSObject {
}
- (BOOL)enqueueErroryRequestWithError:(NSError *_Nullable *)error
                    completionHandler:
                        (nullable CompletionHandler)completionHandler;
- (BOOL)enqueueSyncSuccessfulErroryRequestWithError:(NSError *_Nullable *)error
                                  completionHandler:(nullable CompletionHandler)
                                                        completionHandler;
- (BOOL)enqueueAsyncSuccessfulErroryRequestWithError:(NSError *_Nullable *)error
                                   completionHandler:
                                       (nullable CompletionHandler)
                                           completionHandler;
@end

#pragma clang assume_nonnull end
