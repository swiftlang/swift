#ifndef SWIFT_TEST_OBJC_CLASSES_H
#define SWIFT_TEST_OBJC_CLASSES_H

#import <Foundation/Foundation.h>

@interface FirstClass : NSObject
@property void *x;
@end

@interface SecondClass : NSObject
@property void *x;
@property void *y;
@end

@interface ThirdClass : NSObject
@property void *x;
@property void *y;
@property void *z;
@end

// Direct access to FirstClass for tests that need it.

static inline void *FirstClassPointerRaw(void) {
  extern char OBJC_CLASS_$_FirstClass;
  return &OBJC_CLASS_$_FirstClass;
}

#endif
