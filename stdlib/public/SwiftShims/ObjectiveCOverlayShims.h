//===--- ObjectiveCOverlayShims.h -------------------------------*- C++ -*-===//
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

#ifndef SWIFT_STDLIB_SHIMS_OBJECTIVEC_OVERLAY_H
#define SWIFT_STDLIB_SHIMS_OBJECTIVEC_OVERLAY_H

static inline void *_swift_objc_autoreleasePoolPush() {
  extern void *objc_autoreleasePoolPush();
  return objc_autoreleasePoolPush();
}
static inline void _swift_objc_autoreleasePoolPop(void *pool) {
  extern void objc_autoreleasePoolPop(void *);
  objc_autoreleasePoolPop(pool);
}

#endif // SWIFT_STDLIB_SHIMS_OBJECTIVEC_OVERLAY_H

