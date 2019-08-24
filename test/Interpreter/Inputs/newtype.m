
#include <stdio.h>
#include "newtype.h"

@implementation NSMyObject

@synthesize lifetimeTracked;

-(instancetype)init {
  self = [super init];
  if (!self) {
    return nil;
  }
  self.lifetimeTracked = nil;
  return self;
}

- (void)print {
  printf("I am alive?!\n");
  fflush(stdout);
}

@end
