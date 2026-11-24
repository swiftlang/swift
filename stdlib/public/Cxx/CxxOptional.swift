//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public protocol CxxOptional<Wrapped>: ExpressibleByNilLiteral, ~Escapable {
  associatedtype Wrapped: ~Escapable

  @_lifetime(immortal)
  init()

  @_lifetime(copy value)
  init(_ value: Wrapped)

  func __convertToBool() -> Bool

  var pointee: Wrapped {
    @_lifetime(borrow self)
    get
  }
}

extension CxxOptional where Self: ~Escapable, Wrapped: ~Escapable {
  @_lifetime(immortal)
  public init(nilLiteral: ()) {
    self.init()
  }

  @inlinable
  public var hasValue: Bool {
    get {
      return __convertToBool()
    }
  }

  @inlinable
  public var value: Wrapped? {
    @_lifetime(copy self)
    get {
      guard hasValue else { return nil }
      // 'pointee' projects the wrapped value out of 'self', but the value
      // itself was copied into 'self', so it shares the lifetime of 'self'.
      return unsafe _cxxOverrideLifetime(pointee, copying: self)
    }
    @_lifetime(self: copy newValue)
    set(newValue) {
      if let newValueNonNull = newValue {
        self = Self.init(newValueNonNull)
      } else {
        self = Self.init()
      }
    }
  }
}

extension Optional where Wrapped: ~Escapable {
  @inlinable
  @_lifetime(copy value)
  public init<O: CxxOptional<Wrapped> & ~Escapable>(fromCxx value: borrowing O)
  where O.Wrapped: ~Escapable {
    self = value.value
  }
}
