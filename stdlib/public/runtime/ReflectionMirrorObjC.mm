//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Config.h"

#if SWIFT_OBJC_INTEROP

#include "Private.h"
#include <Foundation/Foundation.h>
#include <objc/objc.h>
#include <objc/runtime.h>

using namespace swift;

// Declare the debugQuickLookObject selector.
@interface DeclareSelectors
- (id)debugQuickLookObject;
@end

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
id swift::_quickLookObjectForPointer(void *value) {
  id object = [*reinterpret_cast<const id *>(value) retain];
  if ([object respondsToSelector:@selector(debugQuickLookObject)]) {
    id quickLookObject = [object debugQuickLookObject];
    [quickLookObject retain];
    [object release];
    return quickLookObject;
  }

  return object;
}

#endif
