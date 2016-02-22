#include "ObjCClasses.h"
#include <stdio.h>

@implementation HasHiddenIvars
@synthesize x;
@synthesize y;
@synthesize z;
@synthesize t;
@end

@implementation NilError
+ (BOOL) throwIt: (NSError**) error {
  return 0;
}
@end

@implementation Container
- (id)initWithObject:(id)anObject {
  if ((self = [super init]) != nil) {
    self.object = anObject;
  }
  return self;
}

- (void)processObjectWithBlock:(void (^)(id))block {
  block(self.object);
}

- (void)getObjectWithBlock:(id (^)())block {
  self.object = block();
}

@synthesize object;

- (id)initWithCat1:(id)anObject {
  return [self initWithObject:anObject];
}

- (id)getCat1 {
  return self.object;
}

- (void)setCat1:(id)obj {
  self.object = obj;
}

- (id)cat1Property {
  return self.object;
}

- (void)setCat1Property:(id)prop {
  self.object = prop;
}

@end

@implementation SubContainer
@end

@implementation NestedContainer
@end

@implementation StringContainer
@end

@implementation CopyingContainer
@end

@implementation Animal
- (void)makeNoise {
  printf("eep\n");
}
@end

@implementation Dog
- (void)makeNoise {
  printf("woof\n");
}
@end

@implementation AnimalContainer
@end

