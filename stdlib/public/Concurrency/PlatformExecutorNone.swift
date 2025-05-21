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

@available(SwiftStdlib 6.2, *)
public struct _PlatformExecutorFactory: _ExecutorFactory {
  public static let mainExecutor: any _MainExecutor = _DummyMainExecutor()
  public static let defaultExecutor: any TaskExecutor = _DummyTaskExecutor()
}
