// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -enable-builtin-module \
// RUN:   -module-name test \
// RUN:   -define-availability "Span 0.1:macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, visionOS 9999" \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature AddressableParameters

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_AddressableParameters

// Test diagnostic output for interesting corner cases. Similar to semantics.swift, but this tests corner cases in the
// implementation as opposed to basic language rules.

import Builtin

struct Borrow<T: ~Copyable>: Copyable, ~Escapable {
  let pointer: UnsafePointer<T>

  @_lifetime(borrow value)
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
  @_lifetime(immortal)
  public init() {}
}

class C {}

struct Holder {
  var c: C? = nil
}

// Generic non-Escapable for indirect values.
struct GNE<T> : ~Escapable {
  let t: T
  @_lifetime(borrow t)
  init(t: borrowing T) { self.t = copy t }
}  

@_silgen_name("forward")
@_lifetime(copy arg)
func forward<T>(_ arg: GNE<T>) -> GNE<T>

@_silgen_name("getGeneric")
@_lifetime(borrow holder)
func getGeneric<T: ~Escapable>(_ holder: borrowing Holder, _: T.Type) -> T

func mutate(_: inout Holder) {}

// Test that conditionally returning an Optional succeeds.
//
// See scope_fixup.sil: testReturnPhi.
@available(Span 0.1, *)
extension Array {
  @_lifetime(&self)
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
@_lifetime(&array)
func getImmutableSpan(_ array: inout [Int]) -> Span<Int> {
 return array.span
}

struct NCInt: ~Copyable {
  var i: Int

  @_lifetime(borrow self)
  func getNE() -> NEInt {
    NEInt(owner: self)
  }
}

public struct NEInt: ~Escapable {
  var i: Int

  @_lifetime(borrow owner)
  init(owner: borrowing NCInt) {
    self.i = owner.i
  }

  @_lifetime(immortal)
  init(immortal i: Int) {
    self.i = i
  }
}

struct TestDeinitCallsAddressor: ~Copyable, ~Escapable {
  let a: Borrow<A>

  deinit {
    useA(a[])
  }
}

// Test a borrowed dependency on an address
@_lifetime(immortal)
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
    @_lifetime(borrow self)
    get { ne }

    @_lifetime(&self)
    set {
      ne = newValue
    }
  }
}

struct HasMethods {
  @_lifetime(borrow self)
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

// =============================================================================
// Indirect ~Escapable results
// =============================================================================

@_lifetime(copy arg1)
func testIndirectForwardedResult<T>(arg1: GNE<T>) -> GNE<T> {
  forward(arg1)
}

@_lifetime(copy arg1)
func testIndirectNonForwardedResult<T>(arg1: GNE<T>, arg2: GNE<T>) -> GNE<T> {
  // expected-error @-1{{lifetime-dependent variable 'arg2' escapes its scope}}
  // expected-note  @-2{{it depends on the lifetime of argument 'arg2'}}
  forward(arg2) // expected-note {{this use causes the lifetime-dependent value to escape}}
}

func testIndirectClosureResult<T>(f: () -> GNE<T>) -> GNE<T> {
  f()
  // expected-error @-1{{lifetime-dependent variable '$return_value' escapes its scope}}
  // expected-note  @-3{{it depends on the lifetime of argument '$return_value'}}
  // expected-note  @-3{{this use causes the lifetime-dependent value to escape}}
}

// =============================================================================
// Coroutines
// =============================================================================

// Test _read of a noncopyable type with a dead-end (end_borrow)
//
// rdar://153479358 (Compiler crash when force-unwrapping optional ~Copyable type)
class ClassStorage {
  private var nc: NCInt?

  init(nc: consuming NCInt?) {
    self.nc = nc
  }

  func readNoncopyable() {
    let ne = self.nc!.getNE()
    _ = ne
  }
}

// =============================================================================
// Immortal
// =============================================================================

@_lifetime(immortal)
func testVoid() -> NEInt {
  let ne = NEInt(immortal: 3)
  return _overrideLifetime(ne, borrowing: ())
}

// =============================================================================
// Optional
// =============================================================================

// A view that contains some arbitrary opaque type, making it address-only, but also depends on an unsafe pointer.
public struct AddressOnlyMutableView<T> : ~Copyable, ~Escapable {
  let t: T

  // placeholder
  @_lifetime(borrow holder)
  init(holder: borrowing Holder, t: T) { self.t = t }

  mutating func modify() {}
}

// Return an address-only optional view (requiring switch_enum_addr).
@_silgen_name("addressOnlyMutableView")
@_lifetime(&holder)
func addressOnlyMutableView<T>(holder: inout Holder, with t: T) -> AddressOnlyMutableView<T>?

// rdar://151231236 ([~Escapable] Missing 'overlapping acceses' error when called from client code, but exact same code
// produces error in same module)
//
// Extend the access scope for &holder across the switch_enum_addr required to unwrap Optional<AddressOnlyMutableView>.
func testSwitchAddr<T>(holder: inout Holder, t: T) {
  // mutableView must be a 'var' to require local variable data flow.
  var mutableView = addressOnlyMutableView(holder: &holder, with: t)! // expected-error {{overlapping accesses to 'holder', but modification requires exclusive access; consider copying to a local variable}}
  mutate(&holder) // expected-note {{conflicting access is here}}
  mutableView.modify()
}

// =============================================================================
// Throwing
// =============================================================================

@available(Span 0.1, *)
func mutableSpanMayThrow(_: borrowing MutableSpan<Int>) throws {}

@available(Span 0.1, *)
func testSpanMayThrow(buffer: inout [Int]) {
  let bufferSpan = buffer.mutableSpan
  try! mutableSpanMayThrow(bufferSpan)
}
