//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import Swift

@available(SwiftStdlib 5.9, *)
public struct ObservationToken: Hashable, Sendable {
  var rawValue: UInt64
  static let generator = ManagedCriticalState(UInt64(0))

  public init() {
    rawValue = ObservationToken.generator.withCriticalRegion { value in
      defer { value &+= 1 }
      return value
    }
  }
}
