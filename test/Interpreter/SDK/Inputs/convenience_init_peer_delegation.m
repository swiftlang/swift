#import "convenience_init_peer_delegation.h"
#include <objc/runtime.h>
#include <stdio.h>

NSInteger baseCounter = 0;
NSInteger subCounter = 0;

@implementation Base
+ (nullable id)allocWithZone:(NSZone *)zone {
  if (self == [Base class])
    ++baseCounter;
  else if (self == [Sub class])
    ++subCounter;
  return [super allocWithZone: zone];
}

- (nonnull instancetype)init {
  fputs("init ", stdout);
  puts(class_getName([self class]));
  return self;
}

- (nonnull instancetype)initConveniently {
  puts(__FUNCTION__);
  return [self init];
}

+ (nonnull instancetype)baseWithConvenientFactory:(_Bool)unused {
  puts(__FUNCTION__);
  return [[self alloc] init];
}

+ (nonnull Base *)baseWithNormalFactory:(_Bool)unused {
  puts(__FUNCTION__);
  return [[Base alloc] init];
}
@end

@implementation Sub
@end
