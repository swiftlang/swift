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

#define SWIFT_TSD_ALLOC_BASE 1024
#define SWIFT_TSD_RAW_ALLOC_BASE 1536
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

#endif // SWIFT_RUNTIME_FASTENTRYPOINTS_H

