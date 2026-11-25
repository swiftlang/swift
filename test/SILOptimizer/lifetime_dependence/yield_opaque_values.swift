// RUN: %target-swift-frontend -emit-sil %s -o /dev/null \
// RUN:   -enable-sil-opaque-values \
// RUN:   -sil-verify-all \
// RUN:   -enable-experimental-feature Lifetimes

// RUN: %target-swift-frontend -emit-sil %s -o /dev/null \
// RUN:   -enable-sil-opaque-values \
// RUN:   -enable-library-evolution \
// RUN:   -sil-verify-all \
// RUN:   -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_Lifetimes

// A non-Escapable value yielded by a read coroutine depends on the coroutine.
// With opaque values, the dependence is marked on the borrowed address-only
// yield without copying it.

public protocol P: ~Copyable, ~Escapable {
  associatedtype Wrapped: ~Copyable, ~Escapable

  var pointee: Wrapped {
    @_lifetime(borrow self)
    get
  }
}

extension P where Self: ~Copyable & ~Escapable, Wrapped: Copyable & ~Escapable {
  public var value: Wrapped {
    @_lifetime(borrow self)
    get { pointee }
  }
}

extension P where Self: ~Copyable & ~Escapable, Wrapped: ~Copyable & ~Escapable {
  public borrowing func borrowPointee() -> Bool {
    take(pointee)
  }
}

func take<T: ~Copyable & ~Escapable>(_: borrowing T) -> Bool { true }
