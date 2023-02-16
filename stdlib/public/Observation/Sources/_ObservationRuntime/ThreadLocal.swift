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

@_silgen_name("_swift_observation_tracking_key")
func _trackingKey() -> UInt64

@_silgen_name("_swift_observation_transaction_key")
func _transactionKey() -> UInt64

@_silgen_name("_swift_observation_tls_get")
func _tlsGet(_ key: UInt64) -> UnsafeMutableRawPointer?

@_silgen_name("_swift_observation_tls_set")
func _tlsSet(_ key: UInt64, _ value: UnsafeMutableRawPointer?)

@available(SwiftStdlib 5.9, *)
public struct ThreadLocal {
  public struct Key {
    var raw: UInt64
    
    init(raw: UInt64) {
      self.raw = raw
    }
  }

  public static let trackingKey = ThreadLocal.Key(raw: _trackingKey())
  public static let transactionKey = ThreadLocal.Key(raw: _transactionKey())

  public static subscript(_ key: Key) -> UnsafeMutableRawPointer? {
    get {
      return _tlsGet(key.raw)
    }
    set {
      _tlsSet(key.raw, newValue)
    }
  }
}
