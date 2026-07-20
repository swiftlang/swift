#ifndef TEST_INTEROP_CXX_ENUM_INPUTS_CF_OPTIONS_TYPEDEF_ALIAS_H
#define TEST_INTEROP_CXX_ENUM_INPUTS_CF_OPTIONS_TYPEDEF_ALIAS_H

#include "CFAvailability.h"

typedef NS_OPTIONS(unsigned long, NSPropertyListMutabilityOptions) {
  NSPropertyListImmutable = 0,
  NSPropertyListMutableContainers = 1,
  NSPropertyListMutableContainersAndLeaves = 2,
};

typedef NSPropertyListMutabilityOptions NSPropertyListReadOptions;

typedef NSPropertyListReadOptions NSPropertyListReadOptions2;
typedef NSPropertyListReadOptions2 NSPropertyListReadOptions3;

@interface PlistReader
- (void)readWithOptions:(NSPropertyListReadOptions)options;
@end

#endif // TEST_INTEROP_CXX_ENUM_INPUTS_CF_OPTIONS_TYPEDEF_ALIAS_H
