#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_CONSTRUCTORS_OBJC_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_CONSTRUCTORS_OBJC_H

#import <Foundation.h>

struct ConstructorWithNSArrayParam {
  ConstructorWithNSArrayParam(NSArray *array) {}
};

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_CONSTRUCTORS_OBJC_H
