//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <objc/objc-api.h>

#include "swift/Runtime/Config.h"

OBJC_EXPORT
void *objc_autoreleasePoolPush(void);

OBJC_EXPORT
void objc_autoreleasePoolPop(void *);

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C" void *_swift_objc_autoreleasePoolPush(void) {
  return objc_autoreleasePoolPush();
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C" void _swift_objc_autoreleasePoolPop(void *context) {
  return objc_autoreleasePoolPop(context);
}

