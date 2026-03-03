#include "rdar80704984_3.h"

#pragma clang assume_nonnull begin

@implementation PFXObject

- (BOOL)enqueueFailingRequestWithData:(nullable NSMutableData *)data
                                error:(NSError *_Nullable *)error
                    completionHandler:
                        (nullable CompletionHandler)completionHandler {
  *error = [[NSError alloc] initWithDomain:@"d" code:1 userInfo:nil];
  return NO;
}
- (BOOL)enqueuePassingRequestWithData:(nullable NSMutableData *)data
                                error:(NSError *_Nullable *)error
                    completionHandler:
                        (nullable CompletionHandler)completionHandler {
  dispatch_async(dispatch_get_main_queue(), ^{
    completionHandler(0, 2);
  });
  return YES;
}

- (BOOL)enqueueFailingRequestWithData:(nullable NSMutableData *)data
                    completionTimeout:(NSTimeInterval)completionTimeout
                                error:(NSError *_Nullable *)error
                    completionHandler:
                        (nullable CompletionHandler)completionHandler {
  *error = [[NSError alloc] initWithDomain:@"d" code:2 userInfo:nil];
  return NO;
}
- (BOOL)enqueuePassingRequestWithData:(nullable NSMutableData *)data
                    completionTimeout:(NSTimeInterval)completionTimeout
                                error:(NSError *_Nullable *)error
                    completionHandler:
                        (nullable CompletionHandler)completionHandler {
  dispatch_async(dispatch_get_main_queue(), ^{
    completionHandler(0, 3);
  });
  return YES;
}

@end

#pragma clang assume_nonnull end
