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

@available(SwiftStdlibCurrentOS 6.2, *)
public struct PlatformExecutorFactory: ExecutorFactory {
  public static var mainExecutor: any MainExecutor {
    if CoreFoundation.isPresent {
      return CFMainExecutor()
    } else {
      return DispatchMainExecutor()
    }
  }

  public static var defaultExecutor: any TaskExecutor {
    if CoreFoundation.isPresent {
      return CFTaskExecutor()
    } else {
      return DispatchGlobalTaskExecutor()
    }
  }
}

#endif // os(macOS) || os(iOS) || os(tvOS) || os(watchOS) || os(visionOS)
