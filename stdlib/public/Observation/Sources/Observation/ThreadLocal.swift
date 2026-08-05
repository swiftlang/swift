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

#if $Embedded

// In Embedded Swift the thread-local storage lives in the platform abstraction
// layer, which reserves a fixed key for each entry in
// include/swift/Threading/TLSKeys.h. Use the reserved observation key directly
// rather than going through the C++ Threading library, which the Embedded Swift
// standard library does not link against.
//
// This value must stay in sync with swift::tls_key::observation_transaction.
private let _observationTransactionKey = 6

@_extern(c, "_swift_tls_get")
internal func _swift_tls_get(_ key: Int) -> UnsafeMutableRawPointer?

@_extern(c, "_swift_tls_set")
internal func _swift_tls_set(_ key: Int, _ value: UnsafeMutableRawPointer?)

struct _ThreadLocal {
  static var value: UnsafeMutableRawPointer? {
    get {
      return _swift_tls_get(_observationTransactionKey)
    }
    set {
      _swift_tls_set(_observationTransactionKey, newValue)
    }
  }
}

#else

@_silgen_name("_swift_observation_tls_get")
func _tlsGet() -> UnsafeMutableRawPointer?

@_silgen_name("_swift_observation_tls_set")
func _tlsSet(_ value: UnsafeMutableRawPointer?)

@available(SwiftStdlib 5.9, *)
struct _ThreadLocal {
  static var value: UnsafeMutableRawPointer? {
    get {
      return _tlsGet()
    }
    set {
      _tlsSet(newValue)
    }
  }
}

#endif
