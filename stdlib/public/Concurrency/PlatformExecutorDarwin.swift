//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY && (os(macOS) || os(iOS) || os(tvOS) || os(watchOS) || os(visionOS))

import Swift

@available(SwiftStdlib 6.2, *)
public struct _PlatformExecutorFactory: _ExecutorFactory {
  public static var mainExecutor: any _MainExecutor {
    if CoreFoundation.isPresent {
      return _CFMainExecutor()
    } else {
      return _DispatchMainExecutor()
    }
  }

  public static var defaultExecutor: any TaskExecutor {
    if CoreFoundation.isPresent {
      return _CFTaskExecutor()
    } else {
      return _DispatchGlobalTaskExecutor()
    }
  }
}

#endif // os(macOS) || os(iOS) || os(tvOS) || os(watchOS) || os(visionOS)
