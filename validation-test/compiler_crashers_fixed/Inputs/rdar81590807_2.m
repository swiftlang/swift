#include "rdar81590807_2.h"

#pragma clang assume_nonnull begin

@implementation PFXObject
- (void)findAnswerSyncSuccessAsynchronously:
    (void (^)(NSString *_Nullable, NSError *_Nullable))handler
    __attribute__((swift_name("findAnswerSyncSuccess(completionHandler:)"))) {
  handler(@"syncSuccess", NULL);
}
- (void)findAnswerAsyncSuccessAsynchronously:
    (void (^)(NSString *_Nullable, NSError *_Nullable))handler
    __attribute__((swift_name("findAnswerAsyncSuccess(completionHandler:)"))) {
  dispatch_async(dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0), ^{
    handler(@"asyncSuccess", NULL);
  });
}
- (void)findAnswerSyncFailAsynchronously:
    (void (^)(NSString *_Nullable, NSError *_Nullable))handler
    __attribute__((swift_name("findAnswerSyncFail(completionHandler:)"))) {
  handler(NULL, [NSError errorWithDomain:@"syncFail" code:1 userInfo:nil]);
}
- (void)findAnswerAsyncFailAsynchronously:
    (void (^)(NSString *_Nullable, NSError *_Nullable))handler
    __attribute__((swift_name("findAnswerAsyncFail(completionHandler:)"))) {
  dispatch_async(dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0), ^{
    handler(NULL, [NSError errorWithDomain:@"asyncFail" code:2 userInfo:nil]);
  });
}
- (void)findAnswerIncorrectAsynchronously:
    (void (^)(NSString *_Nullable, NSError *_Nullable))handler
    __attribute__((swift_name("findAnswerIncorrect(completionHandler:)"))) {
  handler(NULL, NULL);
}
@end

#pragma clang assume_nonnull end
