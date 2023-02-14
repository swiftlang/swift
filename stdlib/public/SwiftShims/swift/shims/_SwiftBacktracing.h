//===--- _SwiftBacktracing.h - Swift Backtracing Support --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Defines types and support functions for the Swift backtracing code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BACKTRACING_H
#define SWIFT_BACKTRACING_H

#include <inttypes.h>

#ifdef __cplusplus
namespace swift {
extern "C" {
#endif

struct CrashInfo {
  uint64_t crashing_thread;
  uint64_t signal;
  uint64_t fault_address;
  uint64_t mctx;
};

#ifdef __cplusplus
} // extern "C"
} // namespace swift
#endif

#endif // SWIFT_BACKTRACING_H
