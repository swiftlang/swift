#import <Foundation/Foundation.h>

#pragma clang assume_nonnull begin

@interface Clazz : NSObject
-(void)doSomethingMultiResultFlaggyWithCompletionHandler:(void (^)(BOOL, NSString *_Nullable, NSError *_Nullable, NSString *_Nullable))completionHandler __attribute__((swift_async_error(zero_argument, 1)));
@end

#pragma clang assume_nonnull end
