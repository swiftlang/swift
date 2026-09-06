//===--- PlatformExecutorWASI.swift ---------------------------------------===//
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

#if os(WASI)

import Swift

// The default executors for wasm32-unknown-wasip1-threads: the thread-pool
// executors in WASIExecutor.swift.
@_spi(ExperimentalCustomExecutors)
@available(StdlibDeploymentTarget 6.3, *)
public struct PlatformExecutorFactory: ExecutorFactory {
  public static let mainExecutor: any MainExecutor = WASIMainExecutor()
  public static let defaultExecutor: any TaskExecutor = WASIGlobalExecutor()
}

#endif // os(WASI)
