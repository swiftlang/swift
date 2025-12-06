//===--- Concurrency.h - Runtime interface for Distributed module ------*- C++ -*-===//
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
//
// The runtime interface for the distributed actors runtime.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_DISTRIBUTED_H
#define SWIFT_RUNTIME_DISTRIBUTED_H

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"

namespace swift {

#if 1

#define SWIFT_DISTRIBUTED_DEBUG_ENABLED 1
#define SWIFT_DISTRIBUTED_DEBUG_LOG(fmt, ...)                                  \
fprintf(stderr, "[distributed] [%s:%d](%s) " fmt "\n",                         \
__FILE__, __LINE__, __FUNCTION__, __VA_ARGS__)
#else
#define SWIFT_DISTRIBUTED_DEBUG_ENABLED 0
#define SWIFT_DISTRIBUTED_DEBUG_LOG(fmt ...) (void)0
#endif

}
#pragma clang diagnostic pop

#endif
