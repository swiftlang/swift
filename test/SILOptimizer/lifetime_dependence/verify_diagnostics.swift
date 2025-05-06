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

public struct NE : ~Escapable {}

public struct NEImmortal: ~Escapable {
  @lifetime(immortal)
  public init() {}
}

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

// Test diagnostics on implicit accessors.
public struct ImplicitAccessors {
  let c: C

  public var neComputed: NEImmortal {
    get {
      NEImmortal()
    }
    set {
    }
  }
}

public struct NoncopyableImplicitAccessors : ~Copyable & ~Escapable {
  public var ne: NE

  public var neComputedBorrow: NE {
    @lifetime(borrow self)
    get { ne }

    @lifetime(&self)
    set {
      ne = newValue
    }
  }
}

struct HasMethods {
  @lifetime(borrow self)
  func data(index: Int) -> NEImmortal {
    NEImmortal()
  }
}

func testClosureCapture1(_ a: HasMethods) {
  let fn = a.data
  // expected-error @-1{{lifetime-dependent value escapes its scope}}
  // expected-note  @-2{{it depends on a closure capture; this is not yet supported}}
  // expected-note  @-3{{this use causes the lifetime-dependent value to escape}}
  _ = consume fn

  let fn2 = a.data(index:)
  // expected-error @-1{{lifetime-dependent value escapes its scope}}
  // expected-note  @-2{{it depends on a closure capture; this is not yet supported}}
  // expected-note  @-3{{this use causes the lifetime-dependent value to escape}}
  _ = consume fn2

  // FIXME: rdar://150073405 ([SILGen] support synthesized _modify on top of borrowed getters with library evolution)
  //
  // withUnsafePointer is disabled because it generates a reabstraction thunk, which is impossible to diagenose.
  /*
  withUnsafePointer(to: a.data) { fptr in
    // future-error @-1{{lifetime-dependent value escapes its scope}}
    // future-note  @-2{{it depends on a closure capture; this is not yet supported}}
    // future-note  @-3{{this use causes the lifetime-dependent value to escape}}
    }
   */
}
