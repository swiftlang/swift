#include <Foundation/Foundation.h>

#pragma clang assume_nonnull begin

@interface PFXObject : NSObject
- (void)performGetStringIdentityWithCompletionHandler:
    (void (^)(NSString * _Nonnull(^ _Nonnull)(NSString * _Nonnull)))completionHandler;
- (void)performGetStringAppendWithCompletionHandler:
    (void (^)(NSString * _Nonnull(^ _Nonnull)(NSString * _Nonnull, NSString * _Nonnull)))completionHandler;
- (void)performGetIntegerIdentityWithCompletionHandler:
    (void (^)(NSInteger(^ _Nonnull)(NSInteger)))completionHandler;
- (void)performGetIntegerSubtractWithCompletionHandler:
    (void (^)(NSInteger(^ _Nonnull)(NSInteger, NSInteger)))completionHandler;
- (void)performGetUIntegerIdentityWithCompletionHandler:
    (void (^)(NSUInteger(^ _Nonnull)(NSUInteger)))completionHandler;
- (void)performGetUIntegerAddWithCompletionHandler:
    (void (^)(NSUInteger(^ _Nonnull)(NSUInteger, NSUInteger)))completionHandler;
@end

#pragma clang assume_nonnull end

