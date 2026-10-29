// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -strict-memory-safety \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature AddressableTypes

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_AddressableTypes

// An optional chain whose result is non-Escapable can depend on the chain's
// base, as `f(s.utf8Span)` depends on the base `s: String`.

struct View: ~Escapable {
  var p: Int

  @_lifetime(borrow base)
  init(_ base: borrowing Base) { p = 0 }

  @_lifetime(borrow base)
  init(_ base: borrowing NoncopyableBase) { p = 0 }

  @_lifetime(borrow base)
  init<T: ~Copyable>(_ base: borrowing Box<T>) { p = 0 }

  var isEmpty: Bool { p == 0 }
}

@_addressableForDependencies
struct Base {
  var id: String

  var view: View {
    @_lifetime(borrow self)
    borrowing get { View(self) }
  }
}

@_addressableForDependencies
struct NoncopyableBase: ~Copyable {
  var id: String

  var view: View {
    @_lifetime(borrow self)
    borrowing get { View(self) }
  }
}

struct NoncopyableHolder: ~Copyable {
  var base: NoncopyableBase?
}

@_addressableForDependencies
struct Box<T: ~Copyable>: ~Copyable {
  var value: T

  var view: View {
    @_lifetime(borrow self)
    borrowing get { View(self) }
  }
}

struct Window: ~Escapable {
  var p: Int

  @_lifetime(copy view)
  init(_ view: borrowing View) { p = view.p }
}

extension View {
  var window: Window {
    @_lifetime(copy self)
    get { Window(self) }
  }
}

struct Outer {
  var inner: Base?
}

struct Outermost {
  var outer: Outer?
}

func f(_ s: View?) {}

func g(_ s: Window?) {}

func make() -> Base? { Base(id: "") }

func param(_ b: Base?) {
  f(b?.view)
}

func paramBoundToLet(_ b: Base?) {
  let u = b?.view
  f(u)
}

func paramIfLetBound(_ b: Base?) {
  if let u = b?.view {
    f(u)
  }
}

func localLet() {
  let b = make()
  f(b?.view)
}

func localVar() {
  var b = make()
  f(b?.view)
  b = nil
  _ = b
}

func localVarBoundToLet() {
  var b = make()
  b = make()
  let u = b?.view
  f(u)
}

func inoutBase(_ b: inout Base?) {
  f(b?.view)
}

func twoLevel(_ o: Outer?) {
  f(o?.inner?.view)
}

func twoLevelInout(_ o: inout Outer?) {
  f(o?.inner?.view)
}

func threeLevel(_ t: Outermost?) {
  f(t?.outer?.inner?.view)
}

func threeLevelInout(_ t: inout Outermost?) {
  f(t?.outer?.inner?.view)
}

func storedPropertyInout(_ o: inout Outer) {
  f(o.inner?.view)
}

func storedPropertyParam(_ o: Outer) {
  f(o.inner?.view)
}

func consumedInChain(_ b: Base?) -> Bool? {
  b?.view.isEmpty
}

func consumedInChainInout(_ b: inout Base?) -> Bool? {
  b?.view.isEmpty
}

func copyStep(_ b: Base?) {
  g(b?.view.window)
}

func copyStepInout(_ b: inout Base?) {
  g(b?.view.window)
}

func copyStepBindToLet(_ b: Base?) {
  let u = b?.view.window
  g(u)
}

func noncopyableBorrowing(_ b: borrowing NoncopyableBase?) {
  f(b?.view)
}

func noncopyableInout(_ b: inout NoncopyableBase?) {
  f(b?.view)
}

func noncopyableConsuming(_ b: consuming NoncopyableBase?) {
  f(b?.view)
}

func noncopyableLocal() {
  let b: NoncopyableBase? = NoncopyableBase(id: "")
  f(b?.view)
}

func noncopyableHop(_ h: borrowing NoncopyableHolder) {
  f(h.base?.view)
}

func noncopyableGeneric<T: ~Copyable>(_ b: borrowing Box<T>?) {
  f(b?.view)
}
