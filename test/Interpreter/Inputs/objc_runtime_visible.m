#import "objc_runtime_visible.h"

@implementation HiddenClass
- (NSString *)name {
  return @"Beatrice";
}
@end

HiddenClass *createHidden() {
  return [[HiddenClass alloc] init];
}
