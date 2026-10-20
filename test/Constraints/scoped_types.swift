// RUN: %target-typecheck-verify-swift -enable-experimental-feature ScopeRestrictions -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_ScopeRestrictions
// REQUIRES: swift_feature_Lifetimes

// FIXME: These conversions are unchecked casts; this only tests that type
// checking doesn't crash. SILGen and debug info still crash on them.
// TODO: Test that scope refinements are used in the return stmt once those are emitted!

struct MyArray {}

struct MyRef: ~Escapable {
  // FIXME: We shouldn't need to specify @_lifetime when using scopes.
  @_lifetime(immortal)
  init() {}
}

func use(_ r: borrowing MyRef) {}
func first(of a: MyArray) -> @_scoped(&a) MyRef { fatalError() }
func with(a: MyArray, body: (@_scoped(&a) MyRef) -> Void) {}

extension MyArray {
  func ref() -> @_scoped(&self) MyRef { fatalError() }
}

func returnInit(a: MyArray) -> @_scoped(&a) MyRef { return MyRef() }

func passToParam() { use(MyRef()) }

func callScopedClosure(a: MyArray, body: (@_scoped(&a) MyRef) -> Void) {
  body(MyRef())
}

func callElidedClosure(_ body: (MyRef) -> Void) { body(MyRef()) }

func callScoped(arr: MyArray) {
  let r = first(of: arr)
  use(r)
  with(a: arr) { r in use(r) }
  use(arr.ref())
  let f = first(of:)
  use(f(arr))
}

func take(_ r: consuming MyRef) {}
func two(_ x: MyRef, _ y: MyRef) {}
func withRef(_ body: (MyRef) -> Void) {}

func mixScopes(_ c: Bool, a: MyArray) -> @_scoped(&a) MyRef {
  c ? MyRef() : first(of: a)
}

func reassign(a: MyArray) {
  var r = MyRef()
  r = first(of: a)
  use(r)
  take(first(of: a))
  two(first(of: a), MyRef())
  let o: MyRef? = first(of: a)
  _ = o
}

func passFunctions(a: MyArray) {
  withRef(use)
  with(a: a, body: use)
}
