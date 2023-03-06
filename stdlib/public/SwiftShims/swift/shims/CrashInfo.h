//===--- CrashInfo.h - Swift Backtracing Crash Information ------*- C++ -*-===//
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
//  Defines the CrashInfo type that holds information about why the program
//  crashed.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CRASHINFO_H
#define SWIFT_CRASHINFO_H

#include "SwiftStdint.h"

#ifdef __cplusplus
namespace swift {
extern "C" {
#endif

struct CrashInfo {
  __swift_uint64_t crashing_thread;
  __swift_uint64_t signal;
  __swift_uint64_t fault_address;
  __swift_uint64_t mctx;
};

#ifdef __cplusplus
} // extern "C"
} // namespace swift
#endif

#endif // SWIFT_CRASHINFO_H
