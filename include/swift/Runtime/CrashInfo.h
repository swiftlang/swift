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

#include <inttypes.h>

#ifdef __cplusplus
namespace swift {
namespace runtime {
namespace backtrace {
extern "C" {
#endif

struct CrashInfo {
  uint64_t crashing_thread;
  uint64_t signal;
  uint64_t fault_address;

#ifdef __APPLE__
  uint64_t mctx;
#elif defined(__linux__)
  uint64_t thread_list;
#endif
};

#ifdef __linux__

struct memserver_req {
  uint64_t addr;
  uint64_t len;
};

struct memserver_resp {
  uint64_t addr;
  int64_t  len;
  /* Then len bytes of data */
};

struct thread {
  uint64_t next;
  int64_t  tid;
  uint64_t uctx;
};

#endif

#ifdef __cplusplus
} // extern "C"
} // namespace backtrace
} // namespace runtime
} // namespace swift
#endif

#endif // SWIFT_CRASHINFO_H
