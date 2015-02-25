#ifndef SWIFT_TEST_OBJC_CLASSES_H
#define SWIFT_TEST_OBJC_CLASSES_H

#include <Foundation/NSObject.h>

/* This class has instance variables which are not apparent in the
   interface.  Subclasses will need to be slid by the ObjC runtime. */
@interface HasHiddenIvars : NSObject
@property NSInteger count;
@end

#endif
