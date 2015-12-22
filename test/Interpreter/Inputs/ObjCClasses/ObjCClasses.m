#include "ObjCClasses.h"

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
