//===--- FakeStdlib.swift -------------------------------------------------===//
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
//
// A handful of standard library stubs to allow Span.swift and RawSpan.swift
// to be compiled as part of the compatibility shim.
//
//===----------------------------------------------------------------------===//

import Swift

@_alwaysEmitIntoClient @_transparent
internal func _precondition(
  _ condition: @autoclosure () -> Bool, _ message: StaticString = StaticString(),
  file: StaticString = #file, line: UInt = #line
) {
	fatalError()
}

@_alwaysEmitIntoClient @_transparent
internal func _internalInvariantFailure(
  _ message: StaticString = StaticString(),
  file: StaticString = #file, line: UInt = #line
) -> Never {
	fatalError()
}

@unsafe
@_unsafeNonescapableResult
@_alwaysEmitIntoClient
@_transparent
@lifetime(borrow source)
internal func _overrideLifetime<
  T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable
>(
  _ dependent: consuming T, borrowing source: borrowing U
) -> T {
  dependent
}

@unsafe
@_unsafeNonescapableResult
@_alwaysEmitIntoClient
@_transparent
@lifetime(copy source)
internal func _overrideLifetime<
  T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable
>(
  _ dependent: consuming T, copying source: borrowing U
) -> T {
  dependent
}

@unsafe
@_unsafeNonescapableResult
@_alwaysEmitIntoClient
@_transparent
@lifetime(&source)
internal func _overrideLifetime<
  T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable
>(
  _ dependent: consuming T,
  mutating source: inout U
) -> T {
  dependent
}

extension Range {
  @unsafe
  @_alwaysEmitIntoClient
  internal init(_uncheckedBounds bounds: (lower: Bound, upper: Bound)) {
    self.init(uncheckedBounds: bounds)
  }
}

extension Optional {
  @_alwaysEmitIntoClient
  internal var _unsafelyUnwrappedUnchecked: Wrapped {
    get {
      if let x = self {
        return x
      }
      _internalInvariantFailure("_unsafelyUnwrappedUnchecked of nil optional")
    }
  }
}
