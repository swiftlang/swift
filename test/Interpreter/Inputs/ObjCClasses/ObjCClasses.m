#include "ObjCClasses.h"

@implementation HasHiddenIvars
@synthesize count;
@end

@implementation NilError
+ (BOOL) throwIt: (NSError**) error {
  return 0;
}
@end
