#include "rdar80704984.h"

#pragma clang assume_nonnull begin

@implementation PFXObject
- (instancetype)init {
  if (self = [super init]) {
  }
  return self;
}
- (BOOL)enqueueErroryRequestWithError:(NSError *_Nullable *)error
                    completionHandler:
                        (nullable CompletionHandler)completionHandler {
  *error = [[NSError alloc] initWithDomain:@"d" code:1 userInfo:nil];
  return NO;
}
- (BOOL)enqueueSyncSuccessfulErroryRequestWithError:(NSError *_Nullable *)error
                                  completionHandler:(nullable CompletionHandler)
                                                        completionHandler {
  completionHandler(0, 1);
  return YES;
}
- (BOOL)enqueueAsyncSuccessfulErroryRequestWithError:(NSError *_Nullable *)error
                                   completionHandler:
                                       (nullable CompletionHandler)
                                           completionHandler;
{
  dispatch_async(dispatch_get_main_queue(), ^{
    completionHandler(0, 2);
  });
  return YES;
}
@end

#pragma clang assume_nonnull end
