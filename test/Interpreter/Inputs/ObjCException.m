#import "ObjCException.h"

@implementation ExceptionCatcher
- (NSException* _Nullable) tryBlock:(__attribute__((noescape)) void(^_Nonnull)(void))unsafeBlock {
    @try {
        unsafeBlock();
    }
    @catch (NSException *exception) {
        return exception;
    }
    return nil;
}
@end
