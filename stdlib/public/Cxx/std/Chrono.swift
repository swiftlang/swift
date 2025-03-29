//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import CxxStdlibShim

extension std.chrono.seconds {
  @available(SwiftStdlib 5.7, *)
  @_alwaysEmitIntoClient
  public init(_ duration: Duration) {
    let (seconds, _) = duration.components
    self = __swift_interopMakeChronoSeconds(seconds)
  }
}

extension std.chrono.milliseconds {
  @available(SwiftStdlib 5.7, *)
  @_alwaysEmitIntoClient
  public init(_ duration: Duration) {
    let (seconds, attoseconds) = duration.components
    self = __swift_interopMakeChronoMilliseconds(
      seconds * 1_000 + 
      attoseconds / 1_000_000_000_000_000)
  }
}

extension std.chrono.microseconds {
  @available(SwiftStdlib 5.7, *)
  @_alwaysEmitIntoClient
  public init(_ duration: Duration) {
    let (seconds, attoseconds) = duration.components
    self = __swift_interopMakeChronoMicroseconds(
      seconds * 1_000_000 +
      attoseconds / 1_000_000_000_000)
  }
}

extension std.chrono.nanoseconds {
  @available(SwiftStdlib 5.7, *)
  @_alwaysEmitIntoClient
  public init(_ duration: Duration) {
    let (seconds, attoseconds) = duration.components
    self = __swift_interopMakeChronoNanoseconds(
      seconds * 1_000_000_000 +
      attoseconds / 1_000_000_000)
  }
}

@available(SwiftStdlib 5.7, *)
extension Duration {
  @_alwaysEmitIntoClient
  public init(_ seconds: std.chrono.seconds) {
    self = Duration.seconds(seconds.count())
  }

  @_alwaysEmitIntoClient
  public init(_ milliseconds: std.chrono.milliseconds) {
    self = Duration.milliseconds(milliseconds.count())
  }

  @_alwaysEmitIntoClient
  public init(_ microseconds: std.chrono.microseconds) {
    self = Duration.microseconds(microseconds.count())
  }

  @_alwaysEmitIntoClient
  public init(_ nanoseconds: std.chrono.nanoseconds) {
    self = Duration.nanoseconds(nanoseconds.count())
  }
}
