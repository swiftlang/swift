// Non-trivial C++ classes as parameter and result types of
// `@cxx @implementation` functions and methods.

// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs

// REQUIRES: swift_feature_CxxImplementation

import NonTrivial


// A non-trivial C++ class is representable in C++ as a parameter and as a
// result. C++ passes the argument as a temporary that the caller may destroy,
// so the implementation borrows it and copies only when it needs its own value.

@cxx @implementation
func takesTracked(_ t: Tracked) -> Int32 { return t.value }

@cxx @implementation
func takesTwoTracked(_ a: Tracked, _ b: Tracked) -> Int32 { return a.value + b.value }

@cxx @implementation
func copiesTracked(_ t: Tracked) -> Int32 {
  var copy = t
  copy.value += 100
  return copy.value
}

@cxx @implementation
func returnsTracked(_ v: Int32) -> Tracked { return Tracked(v) }

@cxx @implementation
func passesThroughTracked(_ t: Tracked) -> Tracked { return t }

@cxx @implementation
func takesMovable(_ m: Movable) -> Int32 { return m.value }

@cxx @implementation
func returnsMovable(_ v: Int32) -> Movable { return Movable(v) }

// A move-only class can be returned. It cannot be taken by value: its parameter
// needs an explicit ownership modifier, and those are rejected (see below).

@cxx @implementation
func returnsMoveOnly(_ v: Int32) -> MoveOnly { return MoveOnly(v) }

@cxx @implementation
func takesPolymorphic(_ p: Polymorphic) -> Int32 { return p.value + p.tag() }


// Methods

extension Box {
  @cxx @implementation
  func take(_ t: Tracked) -> Int32 { return base + t.value }

  @cxx @implementation
  mutating func add(_ t: Tracked) -> Int32 {
    base += t.value
    return base
  }

  @cxx @implementation
  func produce() -> Tracked { return Tracked(base) }

  @cxx @implementation
  static func wrap(_ v: Int32) -> Tracked { return Tracked(v) }
}


// References to a non-trivial class use the pointer spelling.

@cxx @implementation
func readTracked(_ t: UnsafePointer<Tracked>) -> Int32 { return t.pointee.value }

@cxx @implementation
func bumpTracked(_ t: UnsafeMutablePointer<Tracked>) { t.pointee.value += 1 }

@cxx @implementation
func assignTracked(_ dst: UnsafeMutablePointer<Tracked>, _ src: UnsafePointer<Tracked>) {
  dst.pointee = src.pointee
}


// Rejections

// C++ decides which side destroys a by-value argument, so an explicit ownership
// modifier is rejected: `consuming` would have to copy the argument and
// `borrowing` would skip the destruction the callee owes under the Microsoft ABI.
// expected-error@+2{{global function cannot be marked '@cxx' because parameter 't' of non-trivial C++ class type is 'borrowing'; C++ decides which side destroys such an argument, so the implementation must take it with default ownership}}
@cxx @implementation
func takesTrackedBorrowing(_ t: borrowing Tracked) -> Int32 { return t.value }

// expected-error@+2{{global function cannot be marked '@cxx' because parameter 't' of non-trivial C++ class type is 'consuming'; C++ decides which side destroys such an argument, so the implementation must take it with default ownership}}
@cxx @implementation
func takesTrackedConsuming(_ t: consuming Tracked) -> Int32 { return t.value }

// expected-error@+2{{global function cannot be marked '@cxx' because parameter 'm' of non-trivial C++ class type is 'borrowing'; C++ decides which side destroys such an argument, so the implementation must take it with default ownership}}
@cxx @implementation
func takesMoveOnlyBorrowing(_ m: borrowing MoveOnly) -> Int32 { return m.value }

// expected-error@+2{{global function cannot be marked '@cxx' because parameter 'm' of non-trivial C++ class type is 'consuming'; C++ decides which side destroys such an argument, so the implementation must take it with default ownership}}
@cxx @implementation
func takesMoveOnlyConsuming(_ m: consuming MoveOnly) -> Int32 { return m.value }
