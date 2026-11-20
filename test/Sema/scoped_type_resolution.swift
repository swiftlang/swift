// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t
// RUN: %target-swift-frontend -dump-ast -enable-experimental-feature ScopeRestrictions -enable-experimental-feature Lifetimes %t/types.swift -primary-file %t/resolved.swift -o - | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ScopeRestrictions -enable-experimental-feature Lifetimes %t/types.swift %t/invalid.swift %t/unsupported.swift
// RUN: %target-swift-frontend -typecheck -enable-experimental-feature ScopeRestrictions -enable-experimental-feature Lifetimes %t/types.swift %t/secondary.swift -primary-file %t/primary.swift

// REQUIRES: concurrency
// REQUIRES: swift_feature_ScopeRestrictions
// REQUIRES: swift_feature_Lifetimes

//--- types.swift

struct MyArray {}

// FIXME: An unconditionally non-escapable nominal gets one anonymous scope
// parameter.
// TODO: @_scopeParams(dep) ?
struct MyRef: ~Escapable {
  // Use a lifetime annotation so we can write the type...
  @_lifetime(immortal)
  init() {}
}

enum MyEnumRef: ~Escapable {
  case none
}

struct Container {
  struct NestedRef: ~Escapable {}
}

//--- resolved.swift

// Scopes print as `σ_k`, named in print order across the whole type. Each
// function type lists the scopes it binds in `@_scopeParams`: those it
// declares, in the order they're written, then the accesses of its parameters
// that something refers to without binding them, in parameter order.

// MARK: - No scopes

// CHECK: (func_decl{{.*}}"noScopes(_:_:)" interface_type="(Int, (Int) -> Void) -> Int"
func noScopes(_ x: Int, _ body: (Int) -> Void) -> Int { 0 }

extension MyArray {
  // CHECK: (func_decl{{.*}}"noScopesMethod(_:)" interface_type="(MyArray) -> (Int) -> Int"
  func noScopesMethod(_ x: Int) -> Int { 0 }
}

// MARK: - Access scopes

// CHECK: (func_decl{{.*}}"first(of:)" interface_type="@_scopeParams(σ_0) (@_accessScope(σ_0) MyArray) -> @_scoped(σ_0) MyRef"
func first(of a: MyArray) -> @_scoped(&a) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"second(_:_:)" interface_type="@_scopeParams(σ_0) (borrowing MyArray, borrowing @_accessScope(σ_0) MyArray) -> @_scoped(σ_0) MyRef"
func second(_ a: borrowing MyArray, _ b: borrowing MyArray) -> @_scoped(&b) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"mutable(_:)" interface_type="@_scopeParams(σ_0) (inout @_accessScope(σ_0) MyArray) -> @_scoped(σ_0) MyRef"
func mutable(_ a: inout MyArray) -> @_scoped(&a) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"immortal()" interface_type="() -> @_scoped(immortal) MyRef"
func immortal() -> @_scoped(immortal) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"enumRef(of:)" interface_type="@_scopeParams(σ_0) (@_accessScope(σ_0) MyArray) -> @_scoped(σ_0) MyEnumRef"
func enumRef(of a: MyArray) -> @_scoped(&a) MyEnumRef { fatalError() }

// CHECK: (func_decl{{.*}}"nestedRef(of:)" interface_type="@_scopeParams(σ_0) (@_accessScope(σ_0) MyArray) -> @_scoped(σ_0) Container.NestedRef"
func nestedRef(of a: MyArray) -> @_scoped(&a) Container.NestedRef { fatalError() }

// An access comes after the declared scopes.
// CHECK: (func_decl{{.*}}"accessOfRef(_:)" interface_type="@_scopeParams(σ_0, σ_1) (borrowing @_accessScope(σ_1) @_scoped(σ_0) MyRef) -> @_scoped(σ_1) MyRef"
@_scopeParams(s) func accessOfRef(_ r: borrowing @_scoped(s) MyRef) -> @_scoped(&r) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"accessOfVariadic(_:)" interface_type="@_scopeParams(σ_0) (@_accessScope(σ_0) Int...) -> @_scoped(σ_0) MyRef"
func accessOfVariadic(_ xs: Int...) -> @_scoped(&xs) MyRef { fatalError() }

// Accesses are numbered in parameter order.
// TODO: Canonicalize?
// CHECK: (func_decl{{.*}}"accessOrder(_:_:body:)" interface_type="@_scopeParams(σ_0, σ_1) (borrowing @_accessScope(σ_0) MyArray, borrowing @_accessScope(σ_1) MyArray, (@_scoped(σ_1) MyRef, @_scoped(σ_0) MyRef) -> Void) -> ()"
func accessOrder(_ a: borrowing MyArray, _ b: borrowing MyArray, body: (@_scoped(&b) MyRef, @_scoped(&a) MyRef) -> Void) {}

// MARK: - Binding access scopes

// CHECK: (func_decl{{.*}}"bound(of:)" interface_type="@_scopeParams(σ_0) (@_accessScope(σ_0) MyArray) -> @_scoped(σ_0) MyRef"
@_scopeParams(s) func bound(of a: @_accessScope(s) MyArray) -> @_scoped(s) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"boundBorrowing(of:)" interface_type="@_scopeParams(σ_0) (borrowing @_accessScope(σ_0) MyArray) -> @_scoped(σ_0) MyRef"
@_scopeParams(s) func boundBorrowing(of a: borrowing @_accessScope(s) MyArray) -> @_scoped(s) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"boundInParens(_:)" interface_type="@_scopeParams(σ_0) (borrowing @_accessScope(σ_0) MyArray) -> @_scoped(σ_0) MyRef"
@_scopeParams(s) func boundInParens(_ a: borrowing (@_accessScope(s) MyArray)) -> @_scoped(s) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"longer(_:_:)" interface_type="@_scopeParams(σ_0) (borrowing @_accessScope(σ_0) MyArray, borrowing @_accessScope(σ_0) MyArray) -> @_scoped(σ_0) MyRef"
@_scopeParams(s)
func longer(_ a: borrowing @_accessScope(s) MyArray, _ b: borrowing @_accessScope(s) MyArray) -> @_scoped(s) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"sharedMutation(_:_:)" interface_type="@_scopeParams(σ_0) (inout @_accessScope(σ_0) MyArray, inout @_accessScope(σ_0) MyArray) -> @_scoped(σ_0) MyRef"
@_scopeParams(s)
func sharedMutation(_ a: inout @_accessScope(s) MyArray, _ b: inout @_accessScope(s) MyArray) -> @_scoped(s) MyRef { fatalError() }

// An access can share a scope with a value.
// CHECK: (func_decl{{.*}}"accessAndValue(x:_:)" interface_type="@_scopeParams(σ_0) (@_scoped(σ_0) MyRef, borrowing @_accessScope(σ_0) MyArray) -> ()"
@_scopeParams(s)
func accessAndValue(x: @_scoped(s) MyRef, _ a: borrowing @_accessScope(s) MyArray) {}

// MARK: - Explicit scope parameters

// CHECK: (func_decl{{.*}}"named(x:y:)" interface_type="@_scopeParams(σ_0) (@_scoped(σ_0) MyRef, @_scoped(σ_0) MyRef) -> ()"
@_scopeParams(a) func named(x: @_scoped(a) MyRef, y: @_scoped(a) MyRef) {}

// Declared scopes are numbered in the order they're written.
// CHECK: (func_decl{{.*}}"reordered(x:y:)" interface_type="@_scopeParams(σ_0, σ_1) (@_scoped(σ_1) MyRef, @_scoped(σ_0) MyRef) -> ()"
@_scopeParams(b, a) func reordered(x: @_scoped(a) MyRef, y: @_scoped(b) MyRef) {}

// CHECK: (func_decl{{.*}}"resultOnly()" interface_type="@_scopeParams(σ_0) () -> @_scoped(σ_0) MyRef"
@_scopeParams(a) func resultOnly() -> @_scoped(a) MyRef { fatalError() }

extension Container {
  // CHECK: (constructor_decl{{.*}}"init(x:y:)" interface_type="(Container.Type) -> @_scopeParams(σ_0) (@_scoped(σ_0) MyRef, @_scoped(σ_0) MyRef) -> Container"
  @_scopeParams(a) init(x: @_scoped(a) MyRef, y: @_scoped(a) MyRef) {}
}

// Requirement inference resolves the signature of a generic function without
// its scopes.
// CHECK: (func_decl{{.*}}"generic(_:r:)" "<T>" interface_type="@_scopeParams(σ_0) <T> (T, r: @_scoped(σ_0) MyRef) -> @_scoped(σ_0) MyRef"
@_scopeParams(s) func generic<T>(_ t: T, r: @_scoped(s) MyRef) -> @_scoped(s) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"opaque(r:)" interface_type="@_scopeParams(σ_0) (@_scoped(σ_0) MyRef) -> some Any"
@_scopeParams(s) func opaque(r: @_scoped(s) MyRef) -> some Any { 0 }

// MARK: - Function-typed parameters

// CHECK: (func_decl{{.*}}"withRef(_:)" interface_type="(@_scopeParams(σ_0) (@_scoped(σ_0) MyRef) -> Void) -> ()"
func withRef(_ body: @_scopeParams(s) (@_scoped(s) MyRef) -> Void) {}

// CHECK: (func_decl{{.*}}"withRefNonsending(_:)" interface_type="(nonisolated(nonsending) @_scopeParams(σ_0) (@_scoped(σ_0) MyRef) async -> Void) -> ()"
func withRefNonsending(_ body: nonisolated(nonsending) @_scopeParams(s) (@_scoped(s) MyRef) async -> Void) {}

// CHECK: (func_decl{{.*}}"with(a:body:)" interface_type="@_scopeParams(σ_0) (@_accessScope(σ_0) MyArray, (@_scoped(σ_0) MyRef) -> Void) -> ()"
func with(a: MyArray, body: (@_scoped(&a) MyRef) -> Void) {}

// FIXME: The closure type also gets an inferred lifetime dependence.
// CHECK: (func_decl{{.*}}"make(a:body:)" interface_type="@_scopeParams(σ_0) (@_accessScope(σ_0) MyArray, @_lifetime(captures) () -> @_scoped(σ_0) MyRef) -> ()"
func make(a: MyArray, body: () -> @_scoped(&a) MyRef) {}

// CHECK: (func_decl{{.*}}"withNested(a:body:)" interface_type="@_scopeParams(σ_0) (@_accessScope(σ_0) MyArray, ((@_scoped(σ_0) MyRef) -> Void) -> Void) -> ()"
func withNested(a: MyArray, body: ((@_scoped(&a) MyRef) -> Void) -> Void) {}

// CHECK: (func_decl{{.*}}"withShared(x:body:y:)" interface_type="@_scopeParams(σ_0, σ_1) (@_scoped(σ_0) MyRef, @_scopeParams(σ_2) (@_scoped(σ_2) MyRef, @_scoped(σ_0) MyRef) -> Void, @_scoped(σ_1) MyRef) -> ()"
@_scopeParams(s, t)
func withShared(x: @_scoped(s) MyRef, body: @_scopeParams(u) (@_scoped(u) MyRef, @_scoped(s) MyRef) -> Void, y: @_scoped(t) MyRef) {}

// CHECK: (func_decl{{.*}}"onlyInClosure(body:x:)" interface_type="@_scopeParams(σ_0, σ_1) ((@_scoped(σ_0) MyRef) -> Void, @_scoped(σ_1) MyRef) -> ()"
@_scopeParams(a, b) func onlyInClosure(body: (@_scoped(a) MyRef) -> Void, x: @_scoped(b) MyRef) {}

// A scope bound by an inner function type doesn't affect numbering of other parameters.
// CHECK: (func_decl{{.*}}"withNestedNamed(body:x:)" interface_type="@_scopeParams(σ_0) ((@_scopeParams(σ_1) (@_scoped(σ_1) MyRef) -> Void) -> Void, @_scoped(σ_0) MyRef) -> ()"
@_scopeParams(s) func withNestedNamed(body: (@_scopeParams(t) (@_scoped(t) MyRef) -> Void) -> Void, x: @_scoped(s) MyRef) {}

// CHECK: (func_decl{{.*}}"withNamed(body:)" interface_type="(@_scopeParams(σ_0) (@_scoped(σ_0) MyRef, @_scoped(σ_0) MyRef) -> Void) -> ()"
func withNamed(body: @_scopeParams(b) (@_scoped(b) MyRef, @_scoped(b) MyRef) -> Void) {}

// CHECK: (func_decl{{.*}}"outerNamed(x:body:)" interface_type="@_scopeParams(σ_0) (@_scoped(σ_0) MyRef, (@_scoped(σ_0) MyRef) -> Void) -> ()"
@_scopeParams(a) func outerNamed(x: @_scoped(a) MyRef, body: (@_scoped(a) MyRef) -> Void) {}

// FIXME: The closure type also gets an inferred lifetime dependence, on the
// parameter whose type, scope included, is the result's.
// CHECK: (func_decl{{.*}}"bothNamed(x:body:)" interface_type="@_scopeParams(σ_0) (@_scoped(σ_0) MyRef, @_lifetime(captures, copy 0) @_scopeParams(σ_1) (@_scoped(σ_1) MyRef, @_scoped(σ_0) MyRef) -> @_scoped(σ_1) MyRef) -> ()"
@_scopeParams(a)
func bothNamed(x: @_scoped(a) MyRef, body: @_scopeParams(b) (@_scoped(b) MyRef, @_scoped(a) MyRef) -> @_scoped(b) MyRef) {}

// CHECK: (func_decl{{.*}}"withAccess(_:body:)" interface_type="@_scopeParams(σ_0) (borrowing @_accessScope(σ_0) MyArray, (borrowing @_accessScope(σ_0) MyArray) -> Void) -> ()"
func withAccess(_ a: borrowing MyArray, body: (borrowing @_accessScope(&a) MyArray) -> Void) {}

// The access of a function type's parameter can be bound to a scope of the
// function, to store dependent things from the outer function. In this case,
// body is able to return Refs into `a`, but can also store captures between
// calls.
// FIXME: The closure type also gets an inferred lifetime dependence.
// CHECK: (func_decl{{.*}}"withBorrows(_:body:)" interface_type="@_scopeParams(σ_0) (borrowing @_accessScope(σ_0) MyArray, @_lifetime(captures) (borrowing @_accessScope(σ_0) MyArray) -> @_scoped(σ_0) MyRef) -> ()"
@_scopeParams(s)
func withBorrows(_ a: borrowing @_accessScope(s) MyArray, body: (borrowing @_accessScope(s) MyArray) -> @_scoped(s) MyRef) {}

// Using a scope as an access scope of a parameter and the access scope of an
// inner function type counts as a use.
// CHECK: (func_decl{{.*}}"withNamedAccess(_:body:)" interface_type="@_scopeParams(σ_0) (borrowing @_accessScope(σ_0) MyArray, (borrowing @_accessScope(σ_0) MyArray) -> Void) -> ()"
@_scopeParams(s)
func withNamedAccess(_ a: borrowing @_accessScope(s) MyArray, body: (borrowing @_accessScope(s) MyArray) -> Void) {}

// TODO: Diagnose a scope that only the accesses of a function type's parameters
// are bound to. It is impossible to call body due to parametricity, afaik.
// CHECK: (func_decl{{.*}}"onlyBoundByFunctionType(_:)" interface_type="@_scopeParams(σ_0) ((borrowing @_accessScope(σ_0) MyArray) -> Void) -> ()"
@_scopeParams(s) func onlyBoundByFunctionType(_ body: (borrowing @_accessScope(s) MyArray) -> Void) {}

// CHECK: (func_decl{{.*}}"boundByNestedFunctionType(_:)" interface_type="(@_scopeParams(σ_0) ((borrowing @_accessScope(σ_0) MyArray) -> Void) -> Void) -> ()"
func boundByNestedFunctionType(_ body: @_scopeParams(s) ((borrowing @_accessScope(s) MyArray) -> Void) -> Void) {}

// CHECK: (func_decl{{.*}}"sameAsParam(x:)" interface_type="@_scopeParams(σ_0) (@_scoped(σ_0) MyRef) -> ()"
@_scopeParams(x) func sameAsParam(x: @_scoped(x) MyRef) {}

// CHECK: (func_decl{{.*}}"sameAsOuterParam(x:body:)" interface_type="@_scopeParams(σ_0) (@_scoped(σ_0) MyRef, @_scopeParams(σ_1) (@_scoped(σ_1) MyRef) -> Void) -> ()"
@_scopeParams(s)
func sameAsOuterParam(x: @_scoped(s) MyRef, body: @_scopeParams(x) (@_scoped(x) MyRef) -> Void) {}

// CHECK: (func_decl{{.*}}"shadowsOuter(x:body:)" interface_type="@_scopeParams(σ_0) (@_scoped(σ_0) MyRef, @_scopeParams(σ_1) (@_scoped(σ_1) MyRef) -> Void) -> ()"
@_scopeParams(a)
func shadowsOuter(x: @_scoped(a) MyRef, body: @_scopeParams(a) (@_scoped(a) MyRef) -> Void) {}

// MARK: - Methods

// A method's interface type is  `(Self) -> (Params) -> Result` so `self` is
// bound by the outer function type and the other parameters by the inner one.
extension MyArray {
  // CHECK: (func_decl{{.*}}"ref()" interface_type="@_scopeParams(σ_0) (@_accessScope(σ_0) MyArray) -> () -> @_scoped(σ_0) MyRef"
  func ref() -> @_scoped(&self) MyRef { fatalError() }

  // CHECK: (func_decl{{.*}}"mutableRef()" interface_type="@_scopeParams(σ_0) (inout @_accessScope(σ_0) MyArray) -> () -> @_scoped(σ_0) MyRef"
  mutating func mutableRef() -> @_scoped(&self) MyRef { fatalError() }

  // CHECK: (func_decl{{.*}}"refs(x:y:z:)" interface_type="@_scopeParams(σ_0) (@_accessScope(σ_0) MyArray) -> @_scopeParams(σ_1, σ_2) (@_scoped(σ_1) MyRef, @_scoped(σ_0) MyRef, @_scoped(σ_2) MyRef) -> ()"
  @_scopeParams(s, t) func refs(x: @_scoped(s) MyRef, y: @_scoped(&self) MyRef, z: @_scoped(t) MyRef) {}

  // CHECK: (func_decl{{.*}}"refOf(_:body:)" interface_type="@_scopeParams(σ_0) (@_accessScope(σ_0) MyArray) -> @_scopeParams(σ_1) (borrowing @_accessScope(σ_1) MyArray, (@_scoped(σ_0) MyRef, @_scoped(σ_1) MyRef) -> Void) -> ()"
  func refOf(_ a: borrowing MyArray, body: (@_scoped(&self) MyRef, @_scoped(&a) MyRef) -> Void) {}

  // CHECK: (func_decl{{.*}}"borrowWithSelf(_:)" interface_type="@_scopeParams(σ_0) (@_accessScope(σ_0) MyArray) -> (borrowing @_accessScope(σ_0) MyArray) -> @_scoped(σ_0) MyRef"
  func borrowWithSelf(_ a: borrowing @_accessScope(&self) MyArray) -> @_scoped(&a) MyRef { fatalError() }

  // CHECK: (func_decl{{.*}}"method(x:)" interface_type="(MyArray) -> @_scopeParams(σ_0) (@_scoped(σ_0) MyRef) -> @_scoped(σ_0) MyRef"
  @_scopeParams(a) func method(x: @_scoped(a) MyRef) -> @_scoped(a) MyRef { fatalError() }

  // CHECK: (func_decl{{.*}}"visit(_:)" interface_type="@_scopeParams(σ_0) (@_accessScope(σ_0) MyArray) -> ((borrowing @_accessScope(σ_0) MyArray) -> Void) -> ()"
  func visit(_ visitor: (borrowing @_accessScope(&self) MyArray) -> Void) {}
}

// MARK: - Ownership and isolation

actor MyActor {
  // CHECK: (func_decl{{.*}}"ref(_:)" interface_type="@_scopeParams(σ_0) (isolated @_accessScope(σ_0) MyActor) -> (MyArray) -> @_scoped(σ_0) MyRef"
  func ref(_ a: MyArray) -> @_scoped(&self) MyRef { fatalError() }
}

// CHECK: (func_decl{{.*}}"isolatedParam(_:_:)" interface_type="@_scopeParams(σ_0) (isolated MyActor, @_accessScope(σ_0) MyArray) -> @_scoped(σ_0) MyRef"
func isolatedParam(_ actor: isolated MyActor, _ a: MyArray) -> @_scoped(&a) MyRef { fatalError() }

@MainActor
struct OnMain {
  // CHECK: (func_decl{{.*}}"ref()" interface_type="@_scopeParams(σ_0) (@_accessScope(σ_0) OnMain) -> () -> @_scoped(σ_0) MyRef"
  func ref() -> @_scoped(&self) MyRef { fatalError() }
}

// CHECK: (func_decl{{.*}}"sendingParam(_:_:)" interface_type="@_scopeParams(σ_0) (sending MyArray, @_accessScope(σ_0) MyArray) -> @_scoped(σ_0) MyRef"
func sendingParam(_ a: sending MyArray, _ b: MyArray) -> @_scoped(&b) MyRef { fatalError() }

// `sending` doesn't consume a non-escaping closure, whatever its scopes.
// CHECK: (func_decl{{.*}}"sendingClosure(_:)" interface_type="@_scopeParams(σ_0) (sending @_accessScope(σ_0) () -> Void) -> @_scoped(σ_0) MyRef"
func sendingClosure(_ body: sending () -> Void) -> @_scoped(&body) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"sendingScopedClosure(_:)" interface_type="@_scopeParams(σ_0) (sending @_accessScope(σ_0) @_scopeParams(σ_1) (@_scoped(σ_1) MyRef) -> Void) -> @_scoped(σ_0) MyRef"
func sendingScopedClosure(_ body: sending @_scopeParams(s) (@_scoped(s) MyRef) -> Void) -> @_scoped(&body) MyRef { fatalError() }

typealias Action = () -> Void

// CHECK: (func_decl{{.*}}"sendingAlias(_:)" interface_type="@_scopeParams(σ_0) (sending @_accessScope(σ_0) Action) -> @_scoped(σ_0) MyRef"
func sendingAlias(_ body: sending Action) -> @_scoped(&body) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"inoutSending(_:)" interface_type="@_scopeParams(σ_0) (inout sending @_accessScope(σ_0) MyArray) -> @_scoped(σ_0) MyRef"
func inoutSending(_ a: inout sending MyArray) -> @_scoped(&a) MyRef { fatalError() }

// MARK: - ABI

// Scopes aren't part of the ABI.
@abi(func abiResultScope(_ a: MyArray, _ r: MyRef) -> @_scoped(immortal) MyRef)
@_scopeParams(s) func abiResultScope(_ a: MyArray, _ r: @_scoped(s) MyRef) -> @_scoped(&a) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"abiNoScopes(_:_:)" interface_type="@_scopeParams(σ_0) (@_accessScope(σ_0) MyArray, MyArray) -> @_scoped(σ_0) MyRef"
// CHECK: (decl=func_decl{{.*}}abi_only{{.*}}"abiNoScopes(_:_:)" interface_type="(MyArray, MyArray) -> MyRef"
@abi(func abiNoScopes(_ a: MyArray, _ b: MyArray) -> MyRef)
func abiNoScopes(_ a: MyArray, _ b: MyArray) -> @_scoped(&a) MyRef { fatalError() }

@abi(func abiNestedAccessScope(_ a: borrowing MyArray, _ body: (borrowing MyArray) -> Void))
func abiNestedAccessScope(_ a: borrowing MyArray, _ body: (borrowing @_accessScope(&a) MyArray) -> Void) {}

// MARK: - Unsupported

// FIXME: Diagnose a scope on a type with no scope parameters, like an escapable
// or function type, instead of dropping it. Binding this access for an
// Escapable type is super wrong.
// CHECK: (func_decl{{.*}}"noScopeParams(_:_:_:_:)" interface_type="@_scopeParams(σ_0) (@_accessScope(σ_0) MyArray, MyArray, MyArray, () -> Void) -> ()"
func noScopeParams(_ a: MyArray, _ b: @_scoped(nope) MyArray, _ c: @_scoped(&a) MyArray, _ d: @_scoped(&a) () -> Void) {}

// FIXME: Substituting a type that refers to a scope of an enclosing function
// type under another function type doesn't shift the reference, so that
// function type captures it.
func call<T>(_ x: T, _ f: (T) -> Void) {}

// CHECK-LABEL: (func_decl{{.*}}"capturedBySubstitution(_:_:)"
func capturedBySubstitution(_ a: borrowing MyArray, _ g: @escaping (@_scoped(&a) MyRef) -> Void) {
  // FIXME: Shifted correctly, the type should be `(@escaping (@_scoped(^2.0) MyRef) -> Void, (@escaping (@_scoped(^3.0) MyRef) -> Void) -> Void) -> ()`.
  // CHECK: (declref_expr type="(@escaping (@_scoped(^1.0) MyRef) -> Void, (@escaping (@_scoped(^1.0) MyRef) -> Void) -> Void) -> ()"{{.*}}decl="{{.*}}.call@
  call(g) { inner in _ = inner }
}

// FIXME: An inherited initializer doesn't bind the scope parameters its
// parameters' types refer to.
class InheritedScopesBase {
  @_scopeParams(s) init(_ r: @_scoped(s) MyRef) {}
}
// CHECK-LABEL: (class_decl{{.*}}"InheritedScopes"
// CHECK: (constructor_decl{{.*}}implicit{{.*}}"init(_:)" interface_type="(InheritedScopes.Type) -> (@_scoped(^0.0) MyRef) -> InheritedScopes"
class InheritedScopes: InheritedScopesBase {}

// FIXME: Diagnose the parameters of a memberwise initializer of a
// non-escapable type, which aren't written.
// CHECK-LABEL: (struct_decl{{.*}}"MemberwiseScopes"
// CHECK: (constructor_decl{{.*}}implicit{{.*}}"init(r:)" interface_type="@_lifetime(copy 0) (MemberwiseScopes.Type) -> (MyRef) -> MemberwiseScopes"
struct MemberwiseScopes: ~Escapable {
  var r: MyRef
}

//--- invalid.swift

// MARK: - Declaring scope parameters

@_scopeParams(a, a) func duplicate(x: @_scoped(a) MyRef) {}
// expected-error@-1:18 {{invalid redeclaration of scope parameter 'a'}}
// expected-note@-2:15 {{'a' previously declared here}}

@_scopeParams(a) func unused() {}
// expected-error@-1:15 {{scope parameter 'a' is not used in function signature}}

@_scopeParams(a, b) func unusedSecond(x: @_scoped(a) MyRef) {}
// expected-error@-1:18 {{scope parameter 'b' is not used in function signature}}

func unusedInFunctionType(body: @_scopeParams(b) () -> Void) {}
// expected-error@-1:47 {{scope parameter 'b' is not used in function signature}}

extension Container {
  @_scopeParams(a) init(x: Int) {}
  // expected-error@-1:17 {{scope parameter 'a' is not used in function signature}}
}

// This access already lasts for the body, so the scope isn't really used.
@_scopeParams(s) func onlyBound(_ a: borrowing @_accessScope(s) MyArray) {}
// expected-error@-1:15 {{scope parameter 's' is not used in function signature}}

// The access scopes of the two arrays being the same only matters if the same
// scope is used as the dependency of an ~E type, or to give a function type
// in parameter / result a parameter with the same access. Caller could pass an
// arbitrary scope for s, so it isn't meaningfully used.
@_scopeParams(s) func onlyShared(_ a: borrowing @_accessScope(s) MyArray, _ b: borrowing @_accessScope(s) MyArray) {}
// expected-error@-1:15 {{scope parameter 's' is not used in function signature}}

func onlyBoundInFunctionType(_ body: @_scopeParams(s) (borrowing @_accessScope(s) MyArray) -> Void) {}
// expected-error@-1:52 {{scope parameter 's' is not used in function signature}}

// The function type's own `a` shadows the outer one.
@_scopeParams(a) func shadowedUnused(body: @_scopeParams(a) (@_scoped(a) MyRef) -> Void) {}
// expected-error@-1:15 {{scope parameter 'a' is not used in function signature}}

// MARK: - Elided scopes

func elidedParam(_ r: MyRef) {}
// expected-error@-1:23 {{non-escapable type in a function signature must have its scope written with '@_scoped'}}

func elidedBorrowing(_ r: borrowing MyRef) {}
// expected-error@-1:37 {{non-escapable type in a function signature must have its scope written with '@_scoped'}}

func elidedResult(_ a: MyArray) -> MyRef { fatalError() }
// expected-error@-1:36 {{non-escapable type in a function signature must have its scope written with '@_scoped'}}

func elidedClosureParam(_ body: (MyRef) -> Void) {}
// expected-error@-1:34 {{non-escapable type in a function signature must have its scope written with '@_scoped'}}

func elidedClosureResult(_ body: () -> MyRef) {}
// expected-error@-1:40 {{non-escapable type in a function signature must have its scope written with '@_scoped'}}

// Make sure scope on the function doesn't get applied to the function's parameter.
func elidedInScopedClosure(_ body: @_scoped(immortal) (MyRef) -> Void) {}
// expected-error@-1:56 {{non-escapable type in a function signature must have its scope written with '@_scoped'}}

extension MyArray {
  func elidedMethod(_ r: MyRef) {}
  // expected-error@-1:26 {{non-escapable type in a function signature must have its scope written with '@_scoped'}}

  init(r: MyRef) {}
  // expected-error@-1:11 {{non-escapable type in a function signature must have its scope written with '@_scoped'}}
}

// MARK: - Unknown scopes

@_scopeParams(s) func brokenType(x: Missing, r: @_scoped(s) MyRef, y: @_scoped(s) MyRef) {}
// expected-error@-1:37 {{cannot find type 'Missing' in scope}}

func undeclared(x: @_scoped(a) MyRef) {}
// expected-error@-1:29 {{cannot find scope 'a'}}

func accessUndeclared(_ a: borrowing @_accessScope(s) MyArray) {}
// expected-error@-1:52 {{cannot find scope 's'}}

func accessUnknownParam(_ a: borrowing @_accessScope(&b) MyArray) {}
// expected-error@-1:55 {{cannot find parameter 'b'}}

// TODO: Support the scope of a parameter's type as a shorthand?
func paramScope(_ r: @_scoped(immortal) MyRef, _ s: @_scoped(r) MyRef) {}
// expected-error@-1:62 {{depending on the scope of the type of parameter 'r' is not supported}}

func escapableParamScope(_ a: MyArray) -> @_scoped(a) MyRef { fatalError() }
// expected-error@-1:52 {{depending on the scope of the type of parameter 'a' is not supported}}

func genericParamScope<T: ~Escapable>(_ t: T) -> @_scoped(t) MyRef { fatalError() }
// expected-error@-1:59 {{depending on the scope of the type of parameter 't' is not supported}}

func bindingToParamScope(_ a: borrowing @_accessScope(r) MyArray, _ r: @_scoped(immortal) MyRef) {}
// expected-error@-1:55 {{depending on the scope of the type of parameter 'r' is not supported}}

func noSelf() -> @_scoped(self) MyRef { fatalError() }
// expected-error@-1:27 {{cannot find parameter 'self'}}

func noSelfAccess() -> @_scoped(&self) MyRef { fatalError() }
// expected-error@-1:34 {{cannot find parameter 'self'}}

extension MyRef {
  func selfScope() -> @_scoped(self) MyRef { fatalError() }
  // expected-error@-1:32 {{depending on the scope of the type of parameter 'self' is not supported}}
}

extension MyArray {
  func escapableSelfScope() -> @_scoped(self) MyRef { fatalError() }
  // expected-error@-1:41 {{depending on the scope of the type of parameter 'self' is not supported}}

  static func staticSelfScope() -> @_scoped(self) MyRef { fatalError() }
  // expected-error@-1:45 {{depending on the scope of the type of parameter 'self' is not supported}}

  func accessOfSelfScope(_ a: borrowing @_accessScope(self) MyArray) {}
  // expected-error@-1:55 {{depending on the scope of the type of parameter 'self' is not supported}}
}

// MARK: - Access scopes of consumed parameters

func consumed(_ a: consuming MyArray) -> @_scoped(&a) MyRef { fatalError() }
// expected-error@-1:52 {{consumed parameters have no access scope}}
// expected-note@-2:17 {{'a' is consumed}}

func owned(_ a: __owned MyArray) -> @_scoped(&a) MyRef { fatalError() }
// expected-error@-1:47 {{consumed parameters have no access scope}}
// expected-note@-2:14 {{'a' is consumed}}

func sent(_ a: sending MyArray) -> @_scoped(&a) MyRef { fatalError() }
// expected-error@-1:46 {{consumed parameters have no access scope}}
// expected-note@-2:13 {{'a' is consumed}}

func sentEscaping(_ body: sending @escaping () -> Void) -> @_scoped(&body) MyRef { fatalError() }
// expected-error@-1:70 {{consumed parameters have no access scope}}
// expected-note@-2:21 {{'body' is consumed}}

// Computing the specifier diagnoses the default.
func inoutDefault(_ a: inout MyArray = MyArray()) -> @_scoped(&a) MyRef { fatalError() }
// expected-error@-1:40 {{cannot provide default value to inout parameter 'a'}}

@_scopeParams(s) func accessConsumed(_ a: consuming @_accessScope(s) MyArray) -> @_scoped(s) MyRef { fatalError() }
// expected-error@-1:54 {{consumed parameters have no access scope}}
// expected-note@-2:40 {{'a' is consumed}}

func accessOfConsumed(_ b: consuming MyArray, body: (borrowing @_accessScope(&b) MyArray) -> Void) {}
// expected-error@-1:79 {{consumed parameters have no access scope}}
// expected-note@-2:25 {{'b' is consumed}}

func consumedClosureParam(_ body: (consuming @_accessScope(&body) MyArray) -> Void) {}
// expected-error@-1:47 {{consumed parameters have no access scope}}

extension MyArray {
  init(a: MyArray, r: @_scoped(&a) MyRef) {}
  // expected-error@-1:33 {{consumed parameters have no access scope}}
  // expected-note@-2:8 {{initializer parameters are 'consuming' by default}}

  @_scopeParams(s) init(b: @_accessScope(s) MyArray, r: @_scoped(s) MyRef) {}
  // expected-error@-1:29 {{consumed parameters have no access scope}}
  // expected-note@-2:25 {{initializer parameters are 'consuming' by default}}

  init(r: @_scoped(&self) MyRef) {}
  // expected-error@-1:21 {{'self' has no access scope here}}

  static func staticAccess() -> @_scoped(&self) MyRef { fatalError() }
  // expected-error@-1:43 {{'self' has no access scope here}}

  // Whether `self` has an access only depends on the declaration, so it's
  // diagnosed wherever the scope is written.
  static func staticAccessInTuple(_ x: (@_scoped(&self) MyRef, Int)) {}
  // expected-error@-1:51 {{'self' has no access scope here}}

  static func staticAccessOfEscapable(_ a: @_scoped(&self) MyArray) {}
  // expected-error@-1:54 {{'self' has no access scope here}}

  consuming func consumingAccess() -> @_scoped(&self) MyRef { fatalError() }
  // expected-error@-1:49 {{consumed parameters have no access scope}}
  // expected-note@-2:18 {{'self' is consumed}}

  init(b: borrowing @_accessScope(&self) MyArray) {}
  // expected-error@-1:36 {{'self' has no access scope here}}

  static func staticBinding(_ a: borrowing @_accessScope(&self) MyArray) {}
  // expected-error@-1:59 {{'self' has no access scope here}}

  // The binding is diagnosed once, on the parameter.
  static func staticBindingUsed(_ a: borrowing @_accessScope(&self) MyArray) -> @_scoped(&a) MyRef { fatalError() }
  // expected-error@-1:63 {{'self' has no access scope here}}

  consuming func consumingBinding(_ a: borrowing @_accessScope(&self) MyArray) {}
  // expected-error@-1:65 {{consumed parameters have no access scope}}
  // expected-note@-2:18 {{'self' is consumed}}
}

func noSelfBinding(_ a: borrowing @_accessScope(&self) MyArray) {}
// expected-error@-1:50 {{cannot find parameter 'self'}}

// MARK: - Binding access scopes

@_scopeParams(s) func accessOnResult() -> @_accessScope(s) @_scoped(s) MyRef { fatalError() }
// expected-error@-1:44 {{'@_accessScope' may only be used on parameters}}

@_scopeParams(s) func accessTwice(_ a: borrowing @_accessScope(s) (@_accessScope(s) MyArray)) -> @_scoped(s) MyRef { fatalError() }
// expected-error@-1:69 {{a parameter's access scope cannot be bound more than once}}

func functionTypeAccessTwice(_ body: @_scopeParams(s) (borrowing @_accessScope(s) (@_accessScope(s) MyArray)) -> @_scoped(s) MyRef) {}
// expected-error@-1:85 {{a parameter's access scope cannot be bound more than once}}

@_scopeParams(s) func accessDuplicate(_ a: borrowing @_accessScope(s) @_accessScope(s) MyArray) -> @_scoped(s) MyRef { fatalError() }
// expected-error@-1:71 {{duplicate attribute}}

@_scopeParams(s) func accessOnFunctionResult(body: (borrowing @_accessScope(s) MyArray) -> @_accessScope(s) @_scoped(s) MyRef) {}
// expected-error@-1:93 {{'@_accessScope' may only be used on parameters}}

func accessImmortal(_ a: borrowing @_accessScope(immortal) MyArray) {}
// expected-error@-1:50 {{binding an access scope to 'immortal' is not supported}}

func functionTypeAccessImmortal(_ body: (borrowing @_accessScope(immortal) MyArray) -> Void) {}
// expected-error@-1:66 {{binding an access scope to 'immortal' is not supported}}

// TODO: Support binding a parameter's access to another's, which can be written
// with a scope parameter bound to both instead. Doing this totally correctly is
// a little complicated.
func accessOfOtherParam(_ a: borrowing MyArray, _ b: borrowing @_accessScope(&a) MyArray) {}
// expected-error@-1:79 {{binding an access scope to a parameter's access is not supported}}

func accessOfItself(_ a: borrowing @_accessScope(&a) MyArray) {}
// expected-error@-1:51 {{binding an access scope to a parameter's access is not supported}}

// The function type's scope parameters are declared on it directly, so there's
// only ever one list of them.
func scopeParamsInParens(body: @_scopeParams(a) ((@_scoped(a) MyRef) -> Void)) {}
// expected-error@-1:32 {{'@_scopeParams' only applies to function types}}
// expected-error@-2:60 {{cannot find scope 'a'}}

// Scopes aren't mangled, so they can't distinguish overloads.
func redeclaredAccess(_ a: borrowing MyArray, _ body: (borrowing @_accessScope(&a) MyArray) -> Void) {}
// expected-note@-1:6 {{'redeclaredAccess' previously declared here}}
func redeclaredAccess(_ a: borrowing MyArray, _ body: (borrowing MyArray) -> Void) {}
// expected-error@-1:6 {{invalid redeclaration of 'redeclaredAccess'}}

@_scopeParams(s) func redeclaredResult(_ r: @_scoped(s) MyRef) -> @_scoped(s) MyRef { r }
// expected-note@-1:23 {{'redeclaredResult' previously declared here}}
@_scopeParams(s) func redeclaredResult(_ r: @_scoped(s) MyRef) -> @_scoped(immortal) MyRef { MyRef() }
// expected-error@-1:23 {{invalid redeclaration of 'redeclaredResult'}}

// FIXME: Match overrides and witnesses without regard to scopes, like
// overloads, then check that the scopes of the match are compatible.
class ScopedBase {
  func same(_ a: borrowing MyArray) -> @_scoped(&a) MyRef { fatalError() }
  func otherAccess(_ a: borrowing MyArray, _ b: borrowing MyArray) -> @_scoped(&a) MyRef { fatalError() }
  @_scopeParams(s, t) func otherScopes(_ x: @_scoped(s) MyRef, _ y: @_scoped(t) MyRef) {}
  // expected-note@-1:28 {{potential overridden instance method 'otherScopes' here}}
}

class ScopedDerived: ScopedBase {
  override func same(_ a: borrowing MyArray) -> @_scoped(&a) MyRef { fatalError() }
  override func otherAccess(_ a: borrowing MyArray, _ b: borrowing MyArray) -> @_scoped(&b) MyRef { fatalError() }
  @_scopeParams(s) override func otherScopes(_ x: @_scoped(s) MyRef, _ y: @_scoped(s) MyRef) {}
  // expected-error@-1:34 {{method does not override any method from its superclass}}
}

protocol ScopedRequirements {
  func otherAccess(_ a: borrowing MyArray, _ b: borrowing MyArray) -> @_scoped(&a) MyRef
  @_scopeParams(s, t) func otherScopes(_ x: @_scoped(s) MyRef, _ y: @_scoped(t) MyRef)
}

struct ScopedWitnesses: ScopedRequirements {
  func otherAccess(_ a: borrowing MyArray, _ b: borrowing MyArray) -> @_scoped(&b) MyRef { fatalError() }
  @_scopeParams(s) func otherScopes(_ x: @_scoped(s) MyRef, _ y: @_scoped(s) MyRef) {}
}

//--- unsupported.swift

// TODO: Each of these should be supported eventually.

// MARK: - Outside function signatures

typealias Alias = @_scoped(immortal) MyRef
// expected-error@-1:20 {{'@_scoped' is not supported here}}

func local() {
  let r: @_scoped(immortal) MyRef = MyRef()
  // expected-error@-1:11 {{'@_scoped' is not supported here}}
  _ = r
}

extension MyArray {
  subscript(i: Int) -> @_scoped(&self) MyRef {
  // expected-error@-1:25 {{'@_scoped' is not supported here}}
    @_lifetime(borrow self)
    get { fatalError() }
  }
}

// MARK: - Restrictions

func labeled(_ a: MyArray) -> @_scoped(x: &a) MyRef { fatalError() }
// expected-error@-1:40 {{labeled scope restrictions are not supported}}

func multiple(_ a: MyArray) -> @_scoped(&a, immortal) MyRef { fatalError() }
// expected-error@-1:45 {{multiple scope restrictions are not supported}}

func restricted(_ a: MyArray) -> @_scoped(&a) (@_scoped(immortal) MyRef) { fatalError() }
// expected-error@-1:35 {{scope restrictions cannot be specified more than once}}

func functionTypeParam(_ body: (_ x: @_scoped(immortal) MyRef) -> @_scoped(x) MyRef) {}
// expected-error@-1:76 {{scopes named by function type parameters are not supported}}

func functionTypeParamShadowing(x: @_scoped(immortal) MyRef, _ body: (_ x: @_scoped(immortal) MyRef) -> @_scoped(x) MyRef) {}
// expected-error@-1:114 {{scopes named by function type parameters are not supported}}

func functionTypeParamAccess(_ body: (_ x: borrowing MyArray) -> @_scoped(&x) MyRef) {}
// expected-error@-1:76 {{scopes named by function type parameters are not supported}}

func functionTypeParamInNested(_ body: (_ x: @_scoped(immortal) MyRef, _ f: (@_scoped(immortal) MyRef) -> @_scoped(x) MyRef) -> Void) {}
// expected-error@-1:116 {{scopes named by function type parameters are not supported}}

func functionTypeParamAccessScope(x: borrowing MyArray, _ body: (_ x: borrowing MyArray, _ y: borrowing @_accessScope(&x) MyArray) -> Void) {}
// expected-error@-1:120 {{scopes named by function type parameters are not supported}}

// MARK: - Non-escapable parameters

func tuple(_ refs: (MyRef, Int)) {}
// expected-error@-1:20 {{non-escapable type in this position is not supported when scope restrictions are enabled}}

func optional(_ ref: MyRef?) {}
// expected-error@-1:22 {{non-escapable type in this position is not supported when scope restrictions are enabled}}

func variadic(_ bodies: ((MyRef) -> Void)...) {}
// expected-error@-1:25 {{non-escapable type in this position is not supported when scope restrictions are enabled}}

func variadicScoped(_ bodies: (@_scopeParams(s) (@_scoped(s) MyRef) -> Void)...) {}
// expected-error@-1:33 {{'@_scopeParams' is not supported here}}
// expected-error@-2:51 {{'@_scoped' is not supported here}}

@_scopeParams(s) func packAccess<each T>(_ xs: repeat @_accessScope(s) each T) -> @_scoped(s) MyRef { fatalError() }
// expected-error@-1:56 {{'@_accessScope' is not supported here}}

func packFunctionType<each T>(_ body: @_scopeParams(s) (repeat @_accessScope(s) each T) -> @_scoped(s) MyRef) {}
// expected-error@-1:65 {{'@_accessScope' is not supported here}}

func packScoped<each T>(_ bodies: repeat (@_scopeParams(s) (@_scoped(s) MyRef) -> each T)) {}
// expected-error@-1:44 {{'@_scopeParams' is not supported here}}
// expected-error@-2:62 {{'@_scoped' is not supported here}}

func tupleResult() -> (MyRef, Int) { fatalError() }
// expected-error@-1:23 {{non-escapable type in this position is not supported when scope restrictions are enabled}}

extension MyArray {
  subscript(r: MyRef) -> Int { 0 }
  // expected-error@-1:16 {{non-escapable parameters are only supported in functions and initializers when scope restrictions are enabled}}
}

enum Payload: ~Escapable {
  case ref(MyRef)
  // expected-error@-1:12 {{non-escapable parameters are only supported in functions and initializers when scope restrictions are enabled}}
}

func closureParam() {
  let c = { (r: MyRef) in }
  // expected-error@-1:17 {{non-escapable parameters are only supported in functions and initializers when scope restrictions are enabled}}
  _ = c
}

// MARK: - Typealiases

typealias MyRefAlias = MyRef
// expected-error@-1:24 {{non-escapable type in a typealias is not supported when scope restrictions are enabled}}

typealias Body = (MyRef) -> Void
// expected-error@-1:18 {{non-escapable type in a typealias is not supported when scope restrictions are enabled}}

func aliasParam(_ r: MyRefAlias) {}

// MARK: - Generic arguments

typealias Fn<T: ~Escapable> = (T) -> Void

struct Outer<T: ~Escapable> {
  typealias Alias = (T) -> Void
}

func elidedGenericArg(_ body: Fn<MyRef>) {}
// expected-error@-1:34 {{non-escapable type in a generic argument is not supported when scope restrictions are enabled}}

func elidedParentArg(_ body: Outer<MyRef>.Alias) {}
// expected-error@-1:36 {{non-escapable type in a generic argument is not supported when scope restrictions are enabled}}

func scopedGenericArg(a: MyArray, body: Fn<@_scoped(&a) MyRef>) {}
// expected-error@-1:45 {{'@_scoped' is not supported here}}

func scopedParentArg(a: MyArray, body: Outer<@_scoped(&a) MyRef>.Alias) {}
// expected-error@-1:47 {{'@_scoped' is not supported here}}

func scopedOptional(a: MyArray, body: (Optional<@_scoped(&a) MyRef>) -> Void) {}
// expected-error@-1:50 {{'@_scoped' is not supported here}}

func scopedResultGenericArg(_ a: MyArray) -> Optional<@_scoped(&a) MyRef> { fatalError() }
// expected-error@-1:56 {{'@_scoped' is not supported here}}

@_scopeParams(s) func usedInGenericArg(_ body: Fn<@_scoped(s) MyRef>) {}
// expected-error@-1:52 {{'@_scoped' is not supported here}}

func scopeParamsInGenericArg(_ body: Fn<@_scopeParams(a) () -> Void>) {}
// expected-error@-1:42 {{'@_scopeParams' is not supported here}}

protocol P {}
struct Box<T: P> {}
func requireP<T: P>(_ t: T) {}

// Requirement inference resolves the signature without its scopes, so a scope
// that isn't supported doesn't lose `Box`'s requirement on `T`.
func inferredRequirement<T>(_ b: Box<@_scoped(immortal) T>, _ t: T) { requireP(t) }
// expected-error@-1:39 {{'@_scoped' is not supported here}}

// MARK: - Pack expansions

func scopedPack<each T>(a: MyArray, _ x: repeat (each T, @_scoped(&a) MyRef)) {}
// expected-error@-1:59 {{'@_scoped' is not supported here}}

func elidedPack<each T>(_ x: repeat (each T, MyRef)) {}
// expected-error@-1:30 {{non-escapable type in this position is not supported when scope restrictions are enabled}}

// TODO: Support binding the access of a variadic parameter.
@_scopeParams(s) func boundVariadic(_ xs: @_accessScope(s) Int...) -> @_scoped(s) MyRef { fatalError() }
// expected-error@-1:44 {{'@_accessScope' is not supported here}}

// MARK: - Opaque result types

func elidedInOpaqueResult(_ a: MyArray) -> (MyRef) -> some P { fatalError() }
// expected-error@-1:44 {{non-escapable type in this position is not supported when scope restrictions are enabled}}

func elidedBesideOpaqueResult(_ a: MyArray) -> (MyRef, some P) { fatalError() }
// expected-error@-1:48 {{non-escapable type in this position is not supported when scope restrictions are enabled}}

func scopedInOpaqueResult(_ a: MyArray) -> (@_scoped(&a) MyRef) -> some P { fatalError() }
// expected-error@-1:46 {{'@_scoped' is not supported here}}
// expected-error@-2:77 {{underlying type for opaque result type '(_) -> some P' could not be inferred from return expression}}

extension MyArray {
  func selfScopedInOpaqueResult() -> (@_scoped(&self) MyRef) -> some P { fatalError() }
  // expected-error@-1:40 {{'@_scoped' is not supported here}}
  // expected-error@-2:74 {{underlying type for opaque result type '(_) -> some P' could not be inferred from return expression}}
}

// TODO: Can we not cascade here?
func scopedBesideVariadicOpaque(_ a: borrowing MyArray) -> (@_scoped(&a) MyRef, (some P)...) -> Void { fatalError() }
// expected-error@-1:62 {{'@_scoped' is not supported here}}
// expected-error@-2:82 {{'some' types are only permitted in properties, subscripts, and functions}}

extension MyArray {
  static func staticSelfScopedBesideVariadicOpaque() -> (@_scoped(&self) MyRef, (some P)...) -> Void { fatalError() }
  // expected-error@-1:59 {{'@_scoped' is not supported here}}
  // expected-error@-2:82 {{'some' types are only permitted in properties, subscripts, and functions}}
}

// MARK: - Thrown types

func scopedThrown(a: MyArray, body: () throws((@_scoped(&a) any Error)) -> Void) {}
// expected-error@-1:49 {{'@_scoped' is not supported here}}

// MARK: - Subscripts

extension MyArray {
  subscript(a: @_accessScope(&self) MyArray) -> Int { 0 }
  // expected-error@-1:17 {{'@_accessScope' is not supported here}}
}

typealias ScopedBody = @_scopeParams(a) (MyArray) -> Void
// expected-error@-1:25 {{'@_scopeParams' is not supported here}}

//--- secondary.swift

// Don't diagnose issues in secondary files a second time.
@_scopeParams(s, unused) func inSecondary(_ r: @_scoped(s) MyRef) {}

//--- primary.swift

func usesSecondary() { _ = inSecondary }
