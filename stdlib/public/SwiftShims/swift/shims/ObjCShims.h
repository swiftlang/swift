//===--- ObjCShims.h - Access to libobjc for the core stdlib ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Using the ObjectiveC module in the core stdlib would create a
//  circular dependency, so instead we import these declarations as
//  part of SwiftShims.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_OBJCSHIMS_H
#define SWIFT_STDLIB_SHIMS_OBJCSHIMS_H

#include "SwiftStdint.h"
#include "Visibility.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __OBJC2__

extern void objc_setAssociatedObject(void);
extern void objc_getAssociatedObject(void);
int objc_sync_enter(id _Nonnull object);
int objc_sync_exit(id _Nonnull object);

static void * _Nonnull getSetAssociatedObjectPtr() {
  return (void *)&objc_setAssociatedObject;
}

static void * _Nonnull getGetAssociatedObjectPtr() {
  return (void *)&objc_getAssociatedObject;
}

#endif // __OBJC2__

#ifdef __cplusplus
} // extern "C"
#endif

#endif // SWIFT_STDLIB_SHIMS_COREFOUNDATIONSHIMS_H

