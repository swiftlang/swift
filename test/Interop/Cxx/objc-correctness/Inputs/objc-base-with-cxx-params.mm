#import "objc-base-with-cxx-params.h"

@implementation ObjCBase
- (CxxRecord)getRecord {
    return CxxRecord(100);
}
- (void)processRecord:(CxxRecord)record {
    // base does nothing
}
- (CxxRecord)transformRecord:(CxxRecord)input {
    return CxxRecord(input.value * 2);
}
@end
