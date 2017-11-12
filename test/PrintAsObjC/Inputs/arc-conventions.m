@import Foundation;
#import "swift.h"

int main() {
  @autoreleasepool {
    Test *test = [[Test alloc] init];
    id result = [test initAllTheThings]; // CHECK: method called
    NSCAssert(result != nil, @"failed to get a return value back");
    result = [test initAllTheThings]; // CHECK: method called
    NSCAssert(result != nil, @"failed to get a return value back");
#if !__has_feature(objc_arc)
    [test release];
#endif
  } // CHECK: deinitialized
}
