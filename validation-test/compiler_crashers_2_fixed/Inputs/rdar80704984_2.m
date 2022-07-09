#include "rdar80704984_2.h"

#pragma clang assume_nonnull begin

@implementation PFXObject
- (instancetype)init {
  if (self = [super init]) {
  }
  return self;
}
- (BOOL)failReturnWithError:(NSError *_Nullable *)error
          completionHandler:
              (void (^_Nonnull)(NSError *_Nullable error))completionHandler {
  *error = [NSError errorWithDomain:@"failReturn" code:1 userInfo:nil];
  return NO;
}
- (BOOL)failInvokeSyncWithError:(NSError *_Nullable *)error
              completionHandler:(void (^_Nonnull)(NSError *_Nullable error))
                                    completionHandler {
  completionHandler([NSError errorWithDomain:@"failInvokeSync"
                                        code:2
                                    userInfo:nil]);
  return YES;
}
- (BOOL)failInvokeAsyncWithError:(NSError *_Nullable *)error
               completionHandler:(void (^_Nonnull)(NSError *_Nullable error))
                                     completionHandler {
  dispatch_async(dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0), ^{
    completionHandler([NSError errorWithDomain:@"failInvokeAsync"
                                          code:2
                                      userInfo:nil]);
  });
  return YES;
}
- (BOOL)succeedSyncWithError:(NSError *_Nullable *)error
           completionHandler:
               (void (^_Nonnull)(NSError *_Nullable error))completionHandler {
  completionHandler(nil);
  return YES;
}
- (BOOL)succeedAsyncWithError:(NSError *_Nullable *)error
            completionHandler:
                (void (^_Nonnull)(NSError *_Nullable error))completionHandler {
  dispatch_async(dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0), ^{
    completionHandler(nil);
  });
  return YES;
}
@end

#pragma clang assume_nonnull end
