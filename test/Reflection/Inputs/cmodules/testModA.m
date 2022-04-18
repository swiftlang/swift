#import "testModA.h"

@implementation testModAClass
- (instancetype) init {
    if ((self = [super init]) == nil) return nil;
    self.x = 42;
    return self;
}
@end