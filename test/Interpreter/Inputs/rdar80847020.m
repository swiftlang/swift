#include "rdar80847020.h"

#pragma clang assume_nonnull begin

@implementation Clazz
-(void)doSomethingMultiResultFlaggyWithCompletionHandler:(void (^)(BOOL, NSString *_Nullable, NSError *_Nullable, NSString *_Nullable))completionHandler __attribute__((swift_async_error(zero_argument, 1))) {
    completionHandler(YES, @"hi", NULL, @"bye");
}
@end

#pragma clang assume_nonnull end
