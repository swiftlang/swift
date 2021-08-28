@import Foundation;

@interface ClassWithHandlerMethods
+ (void)firstBoolFlagSuccess:(NSString *_Nonnull)str
                  completion:(void (^_Nonnull)(NSString *_Nullable, BOOL, BOOL,
                                               NSError *_Nullable))handler
    __attribute__((swift_async_error(zero_argument, 2)));

+ (void)secondBoolFlagFailure:(NSString *_Nonnull)str
                   completion:(void (^_Nonnull)(NSString *_Nullable, BOOL, BOOL,
                                                NSError *_Nullable))handler
    __attribute__((swift_async_error(nonzero_argument, 3)));
@end
