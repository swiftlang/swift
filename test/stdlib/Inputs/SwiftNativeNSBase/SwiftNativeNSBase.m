//===--- SwiftNativeNSBase.m - Test __SwiftNativeNS*Base classes -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This file is compiled and run by SwiftNativeNSBase.swift.

#include <Foundation/Foundation.h>
#include <objc/runtime.h>

static int Errors;

#define expectTrue(expr)                                            \
  do {                                                              \
    if (!(expr)) {                                                  \
      printf("%s:%d: not true:  %s\n", __FILE__, __LINE__, #expr);  \
      Errors++;                                                     \
    }                                                               \
  } while (0)

#define expectFalse(expr)                                           \
  do {                                                              \
    if (expr) {                                                     \
      printf("%s:%d: not false: %s\n", __FILE__, __LINE__, #expr);  \
      Errors++;                                                     \
    }                                                               \
  } while (0)

#define fail(format, ...)                                           \
  do {                                                              \
    printf("%s:%d: " format, __FILE__, __LINE__, ##__VA_ARGS__);    \
    Errors++;                                                       \
  } while (0)


BOOL TestSwiftNativeNSBase_RetainCount(id object)
{
  Errors = 0;
  NSUInteger rc1 = [object retainCount];
  id object2 = [object retain];
  expectTrue(object == object2);
  NSUInteger rc2 = [object retainCount];
  expectTrue(rc2 > rc1);
  [object release];
  NSUInteger rc3 = [object retainCount];
  expectTrue(rc3 < rc2);
  return Errors == 0;
}

BOOL TestSwiftNativeNSBase_UnwantedCdtors()
{
  Errors = 0;
  printf("TestSwiftNativeNSBase\n");

  unsigned int classCount;
  Class *classes = objc_copyClassList(&classCount);

  NSMutableSet *expectedClasses =
    [NSMutableSet setWithObjects:
      @"__SwiftNativeNSArrayBase",
      @"__SwiftNativeNSMutableArrayBase",
      @"__SwiftNativeNSDictionaryBase",
      @"__SwiftNativeNSSetBase",
      @"__SwiftNativeNSStringBase",
      @"__SwiftNativeNSEnumeratorBase",
      nil];

  for (unsigned int i = 0; i < classCount; i++) {
    Class cls = classes[i];
    NSString *name = @(class_getName(cls));
    if (! ([name hasPrefix:@"__SwiftNativeNS"] && [name hasSuffix:@"Base"])) {
      continue;
    }
    if ([name isEqual: @"__SwiftNativeNSDataBase"] ||
        [name isEqual: @"__SwiftNativeNSIndexSetBase"]) {
      //These two were removed but are still present when back-deploying
      continue;
    }
    if (! [expectedClasses containsObject:name]) {
      fail("did not expect class %s\n", name.UTF8String);
      continue;
    }

    // cls is some __SwiftNativeNS*Base class
    [expectedClasses removeObject:name];
    printf("checking class %s\n", name.UTF8String);

    // Check for unwanted C++ cdtors (rdar://18950072)
    expectFalse([cls instancesRespondToSelector:sel_registerName(".cxx_construct")]);
    expectFalse([cls instancesRespondToSelector:sel_registerName(".cxx_destruct")]);
  }

  expectTrue(expectedClasses.count == 0);

  printf("TestSwiftNativeNSBase: %d error%s\n",
         Errors, Errors == 1 ? "" : "s");
  return Errors == 0;
}
