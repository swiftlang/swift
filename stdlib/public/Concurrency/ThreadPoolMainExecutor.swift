//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// The main executor of libswift_ConcurrencyThreadPoolExecutor.a. This file is
// the archive's module, _ConcurrencyThreadPoolExecutor, which no program
// imports.

import Swift
import _Concurrency

@_silgen_name("swift_threadPoolEnqueueMain")
func _threadPoolEnqueueMain(_ job: UnownedJob)

/// Runs main-actor jobs on the thread that runs `async` main.
final class ThreadPoolMainExecutor: SerialExecutor {
  static let shared = ThreadPoolMainExecutor()

  func enqueue(_ job: consuming ExecutorJob) {
    _threadPoolEnqueueMain(UnownedJob(job))
  }
}

// Only ThreadPoolGlobalExecutor.cpp calls this.
@used
@_silgen_name("swift_threadPoolGetMainExecutor")
func _threadPoolGetMainExecutor() -> UnownedSerialExecutor {
  return unsafe ThreadPoolMainExecutor.shared.asUnownedSerialExecutor()
}
