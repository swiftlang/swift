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

import Swift

// This platform uses a single, global, CooperativeExecutor
@available(SwiftStdlibCurrentOS 6.2, *)
public struct PlatformExecutorFactory: ExecutorFactory {
  static let executor = CooperativeExecutor()
  public static var mainExecutor: any MainExecutor { executor }
  public static var defaultExecutor: any TaskExecutor { executor }
}
