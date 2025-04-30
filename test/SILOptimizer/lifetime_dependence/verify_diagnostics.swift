// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -enable-builtin-module \
// RUN:   -module-name test \
// RUN:   -define-availability "Span 0.1:macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, visionOS 9999" \
// RUN:   -enable-experimental-feature LifetimeDependence \
// RUN:   -enable-experimental-feature AddressableParameters

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_AddressableParameters

// Test diagnostic output for interesting corner cases. Similar to semantics.swift, but this tests corner cases in the
// implementation as opposed to basic language rules.

import Builtin

struct Borrow<T: ~Copyable>: Copyable, ~Escapable {
  let pointer: UnsafePointer<T>

  @lifetime(borrow value)
  init(_ value: borrowing @_addressable T) {
    pointer = UnsafePointer(Builtin.unprotectedAddressOfBorrow(value))
  }

  subscript() -> T {
    unsafeAddress {
      pointer
    }
  }
}

struct A {}

func useA(_:A){}

struct NE : ~Escapable {}

class C {}
struct Holder {
  var c: C? = nil
}

@_silgen_name("getGeneric")
@lifetime(borrow holder)
func getGeneric<T: ~Escapable>(_ holder: borrowing Holder, _: T.Type) -> T

func mutate(_: inout Holder) {}

// Test that conditionally returning an Optional succeeds.
//
// See scope_fixup.sil: testReturnPhi.
@available(Span 0.1, *)
extension Array {
  @lifetime(&self)
  mutating func getOptionalMutableSpan() -> MutableSpan<Element>? {
    if count == 0 {
      return nil
    }
    return mutableSpan
  }
}

// Test that returning an immutable Span from an inout Array.
//
// See scope_fixup.sil: testNestedModRead.
@available(Span 0.1, *)
@inline(never)
@lifetime(&array)
func getImmutableSpan(_ array: inout [Int]) -> Span<Int> {
 return array.span
}

struct TestDeinitCallsAddressor: ~Copyable, ~Escapable {
  let a: Borrow<A>

  deinit {
    useA(a[])
  }
}

// Test a borrowed dependency on an address
@lifetime(immortal)
public func testGenericDep<T: ~Escapable>(type: T.Type) -> T {
  let holder = Holder()
  let result = getGeneric(holder, type)
  // expected-error @-1{{lifetime-dependent variable 'result' escapes its scope}}
  // expected-note  @-3{{it depends on the lifetime of variable 'holder'}}
  return result
  // expected-note @-1{{this use causes the lifetime-dependent value to escape}}
}
