#include "rdar81421394.h"

#pragma clang assume_nonnull begin
@implementation Ranger
- (instancetype)init {
  if (self = [super init]) {
    NSLog(@"calling init");
    self->_range = NSMakeRange(0, 0);
  }
  return self;
}
- (void)setRange:(NSRange)newRange {
  NSLog(@"%s: %@->%@", __PRETTY_FUNCTION__, NSStringFromRange(_range),
        NSStringFromRange(newRange));
  _range = newRange;
}
- (NSRange)range {
  NSLog(@"%s: %@", __PRETTY_FUNCTION__, NSStringFromRange(_range));
  return _range;
}
@end
#pragma clang assume_nonnull end
