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

// Note: This file is #included in assembly files.

// Allocation cache layout.
// XXX FIXME -- we need to clean this up when the project isn't a secret.
// There are only 256 slots, and the latter half is basically unused. We can
// go lower than 128, but we eventually begin to stomp on other frameworks.
#define ALLOC_CACHE_RESERVED 128
#define ALLOC_CACHE_BUCKETS  64
#ifdef __LP64__
# define SWIFT_TSD_ALLOC_BASE (ALLOC_CACHE_RESERVED*8)
# define SWIFT_TSD_RAW_ALLOC_BASE (SWIFT_TSD_ALLOC_BASE + ALLOC_CACHE_BUCKETS*8)
#else
# define SWIFT_TSD_ALLOC_BASE (ALLOC_CACHE_RESERVED*4)
# define SWIFT_TSD_RAW_ALLOC_BASE (SWIFT_TSD_ALLOC_BASE + ALLOC_CACHE_BUCKETS*4)
#endif

#define RC_OFFSET 0x8
#define RC_INTERVAL 4
#define RC_MASK 0xfffffffc
#define RC_ATOMIC_BIT 0x1
#define RC_DEALLOCATING_BIT 0x2
#define WRC_OFFSET 0xc
#define WRC_INTERVAL 1
#define WRC_MASK 0xffffffff
#define SWIFT_TRYALLOC 0x0001
#define SWIFT_RAWALLOC 0x0002
#define SWIFT_TRYRAWALLOC 0x0003

#if __x86_64__
# define SWIFT_HAVE_FAST_ENTRY_POINTS 1
#elif __i386__  ||  __arm__  ||  __arm64__
# undef  SWIFT_HAVE_FAST_ENTRY_POINTS
#else
# error unknown architecture
#endif

#endif // SWIFT_RUNTIME_FASTENTRYPOINTS_H

