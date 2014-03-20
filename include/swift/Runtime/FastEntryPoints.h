//===--- FastEntryPoints.h - Swift Language Assembly Entry Points ABI -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Swift Language Assembly Entry Points ABI -- offsets of interest
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_FASTENTRYPOINTS_H
#define SWIFT_RUNTIME_FASTENTRYPOINTS_H

#include <TargetConditionals.h>
#include "../../../stdlib/shims/RefCount.h"

#ifdef __LP64__
#define ALLOC_CACHE_COUNT 56
#else
#define ALLOC_CACHE_COUNT 64
#endif

// Note: This file is #included in assembly files.

#define RC_OFFSET 0x8
#define RC_MASK 0xfffffffc
#define RC_ATOMIC_BIT 0x1
#define RC_DEALLOCATING_BIT 0x2
#define WRC_OFFSET 0xc
#define WRC_INTERVAL 1
#define WRC_MASK 0xffffffff
#define SWIFT_TRYALLOC 0x0001

#ifdef SWIFT_HAVE_FAST_ENTRY_POINTS
#error "Do not try to override SWIFT_HAVE_FAST_ENTRY_POINTS"
#endif

//#if __x86_64__ && !TARGET_IPHONE_SIMULATOR
//# define SWIFT_HAVE_FAST_ENTRY_POINTS 1
//#endif

#endif // SWIFT_RUNTIME_FASTENTRYPOINTS_H

