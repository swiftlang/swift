//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Builtin

@available(SwiftStdlib 6.3, *)
@frozen
@unsafe
@_rawLayout(like: Value, movesAsLike)
public struct _Cell<Value: ~Copyable>: ~Copyable {
  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var address: UnsafeMutablePointer<Value> {
    unsafe UnsafeMutablePointer<Value>(_rawAddress)
  }

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal var rawAddress: Builtin.RawPointer {
    Builtin.addressOfRawLayout(self)
  }

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  public var value: Value {
    @_transparent
    unsafeAddress {
      UnsafePointer<Value>(address)
    }

    @_transparent
    nonmutating unsafeMutableAddress {
      address
    }
  }

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init(_ initialValue: consuming Value) {
    unsafe _address.initialize(to: initialValue)
  }

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  deinit {
    unsafe _address.deinitialize(count: 1)
  }
}
