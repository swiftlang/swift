//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

extension String : Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @inlinable // For pre-normal fast paths
  public func hash(into hasher: inout Hasher) {
    // TODO(UTF8 perf): pre-normal checks, fast-paths, etc.

    _guts._normalizedHash(into: &hasher)
  }
}

extension StringProtocol {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    unimplemented_utf8()
  }
}

extension _StringGuts {
  @usableFromInline // @opaque
  @inline(never) // slow-path
  internal func _normalizedHash(into hasher: inout Hasher) {
    // TODO(UTF8 perf): fast-paths, incremental (non-allocating) normalization,
    // etc. This approach is very slow.

    String(self)._normalize().withUnsafeBytes {
      hasher.combine(bytes: $0)
    }
    hasher.combine(0xFF as UInt8) // terminator
  }
}

