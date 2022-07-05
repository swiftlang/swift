#import <Foundation.h>

#pragma clang assume_nonnull begin

@protocol NSButz

- (void)idString:(NSObject *)x replyHandler:(void (^)(id _Nullable reply, NSString *_Nullable errorMessage))replyHandler __attribute__((swift_async(not_swift_private, 2)));
- (void)idStringID:(NSObject *)x replyHandler:(void (^)(id _Nullable reply, NSString *_Nullable errorMessage, id _Nullable reply2))replyHandler __attribute__((swift_async(not_swift_private, 2)));
- (void)stringIDString:(NSObject *)x replyHandler:(void (^)(NSString *_Nullable errorMessage, id _Nullable reply, NSString *_Nullable errorMessage2))replyHandler __attribute__((swift_async(not_swift_private, 2)));
- (void)idStringIDString:(NSObject *)x replyHandler:(void (^)(id _Nullable reply, NSString *_Nullable errorMessage, id _Nullable reply2, NSString *_Nullable errorMessage2))replyHandler __attribute__((swift_async(not_swift_private, 2)));

@end

#pragma clang assume_nonnull end
