//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

@available(StdlibDeploymentTarget 6.3, *)
internal final class EmbeddedMainExecutor: SerialExecutor {
  static let shared = EmbeddedMainExecutor()

  func enqueue(_ job: consuming ExecutorJob) {
    _enqueueOnMain(UnownedJob(job))
  }
}

#if os(WASI) || os(Emscripten)
@available(StdlibDeploymentTarget 6.3, *)
extension EmbeddedMainExecutor: MainExecutor {
  func run() throws {
    _asyncMainDrainQueue()
  }

  func stop() {
    fatalError("The main executor of the Concurrency PAL cannot be stopped")
  }
}
#endif

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_task_getMainExecutorImpl")
internal func getMainExecutor() -> UnownedSerialExecutor {
  return unsafe EmbeddedMainExecutor.shared.asUnownedSerialExecutor()
}
