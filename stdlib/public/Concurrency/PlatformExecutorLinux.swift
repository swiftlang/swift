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

#if !$Embedded && (os(Linux) || os(Android))

import Swift

// The default executors for now are Dispatch-based
@available(StdlibDeploymentTarget 6.2, *)
public struct PlatformExecutorFactory: ExecutorFactory {
  public static let mainExecutor: any MainExecutor = DispatchMainExecutor()
  public static let defaultExecutor: any TaskExecutor
    = DispatchGlobalTaskExecutor()
}

#endif // os(Linux)
