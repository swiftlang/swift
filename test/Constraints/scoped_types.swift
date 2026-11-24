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

@_scopeParams(s) func use(_ r: borrowing @_scoped(s) MyRef) {}
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

func callNamedClosure(_ body: @_scopeParams(s) (@_scoped(s) MyRef) -> Void) {
  body(MyRef())
}

func callScoped(arr: MyArray) {
  let r = first(of: arr)
  use(r)
  with(a: arr) { r in use(r) }
  use(arr.ref())
  let f = first(of:)
  use(f(arr))
}

@_scopeParams(s) func take(_ r: consuming @_scoped(s) MyRef) {}
@_scopeParams(s, t) func two(_ x: @_scoped(s) MyRef, _ y: @_scoped(t) MyRef) {}
func withRef(_ body: @_scopeParams(s) (@_scoped(s) MyRef) -> Void) {}

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

func withMutation(_ a: inout MyArray, _ body: (borrowing @_accessScope(&a) MyArray) -> Void) {}

// Like a type's scopes, a parameter's access scope doesn't stop a conversion.
func convertAccessScopes() {
  let mutations = [withMutation]
  let _: [(inout MyArray, (borrowing MyArray) -> Void) -> ()] = mutations
}
