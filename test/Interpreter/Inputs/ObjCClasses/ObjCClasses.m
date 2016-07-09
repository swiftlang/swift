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


#if __has_feature(objc_class_property)
static int _value = 0;
@implementation ClassWithClassProperty
+ (int)value {
  return _value;
}
+ (void)setValue:(int)newValue {
  _value = newValue;
}
+ (void)reset {
  _value = 0;
}
@end

@implementation ObjCSubclassWithClassProperty
+ (BOOL)optionalClassProp {
  return YES;
}
@end

@implementation PropertyNamingConflict
- (id)prop { return self; }
+ (id)prop { return nil; }
@end

#endif
