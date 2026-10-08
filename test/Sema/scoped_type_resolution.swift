// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t
// RUN: %target-swift-frontend -dump-ast -enable-experimental-feature ScopeRestrictions -enable-experimental-feature Lifetimes %t/types.swift -primary-file %t/resolved.swift -o - | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ScopeRestrictions -enable-experimental-feature Lifetimes %t/types.swift %t/unsupported.swift

// REQUIRES: swift_feature_ScopeRestrictions
// REQUIRES: swift_feature_Lifetimes

//--- types.swift

struct MyArray {}

// FIXME: An unconditionally non-escapable nominal gets one anonymous scope
// parameter.
// TODO: @_forall(dep) ?
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

// Printing scopes as ^depth.index
// Access scopes, then scopes for parameters

// MARK: - Access scopes

// CHECK: (func_decl{{.*}}"first(of:)" interface_type="(MyArray) -> @_scoped(^0.0) MyRef"
func first(of a: MyArray) -> @_scoped(&a) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"second(_:_:)" interface_type="(borrowing MyArray, borrowing MyArray) -> @_scoped(^0.1) MyRef"
func second(_ a: borrowing MyArray, _ b: borrowing MyArray) -> @_scoped(&b) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"mutable(_:)" interface_type="(inout MyArray) -> @_scoped(^0.0) MyRef"
func mutable(_ a: inout MyArray) -> @_scoped(&a) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"immortal()" interface_type="() -> @_scoped(immortal) MyRef"
func immortal() -> @_scoped(immortal) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"enumRef(of:)" interface_type="(MyArray) -> @_scoped(^0.0) MyEnumRef"
func enumRef(of a: MyArray) -> @_scoped(&a) MyEnumRef { fatalError() }

// CHECK: (func_decl{{.*}}"nestedRef(of:)" interface_type="(MyArray) -> @_scoped(^0.0) Container.NestedRef"
func nestedRef(of a: MyArray) -> @_scoped(&a) Container.NestedRef { fatalError() }

// MARK: - Value scopes of non-escapable parameters

// CHECK: (func_decl{{.*}}"pass(_:)" interface_type="(@_scoped(^0.1) MyRef) -> @_scoped(^0.1) MyRef"
func pass(_ r: MyRef) -> @_scoped(r) MyRef { fatalError() }

// Scope parameters are numbered by first occurrence.
// CHECK: (func_decl{{.*}}"pick(x:y:)" interface_type="(@_scoped(^0.2) MyRef, @_scoped(^0.3) MyRef) -> @_scoped(^0.3) MyRef"
func pick(x: MyRef, y: MyRef) -> @_scoped(y) MyRef { fatalError() }

// CHECK: (func_decl{{.*}}"share(x:y:)" interface_type="(@_scoped(^0.2) MyRef, @_scoped(^0.2) MyRef) -> ()"
func share(x: MyRef, y: @_scoped(x) MyRef) {}

// CHECK: (func_decl{{.*}}"shareKinds(x:y:z:)" interface_type="(@_scoped(^0.3) MyEnumRef, @_scoped(^0.3) MyRef, @_scoped(^0.4) Container.NestedRef) -> ()"
func shareKinds(x: MyEnumRef, y: @_scoped(x) MyRef, z: Container.NestedRef) {}

// CHECK: (func_decl{{.*}}"shareThenElide(x:y:z:)" interface_type="(@_scoped(^0.3) MyRef, @_scoped(^0.3) MyRef, @_scoped(^0.4) MyRef) -> ()"
func shareThenElide(x: MyRef, y: @_scoped(x) MyRef, z: MyRef) {}

// MARK: - Function-typed parameters

// CHECK: (func_decl{{.*}}"withRef(_:)" interface_type="((@_scoped(^0.1) MyRef) -> Void) -> ()"
func withRef(_ body: (MyRef) -> Void) {}

// CHECK: (func_decl{{.*}}"with(a:body:)" interface_type="(MyArray, (@_scoped(^1.0) MyRef) -> Void) -> ()"
func with(a: MyArray, body: (@_scoped(&a) MyRef) -> Void) {}

// FIXME: The closure type also gets an inferred lifetime dependence.
// CHECK: (func_decl{{.*}}"make(a:body:)" interface_type="(MyArray, @_lifetime(captures) () -> @_scoped(^1.0) MyRef) -> ()"
func make(a: MyArray, body: () -> @_scoped(&a) MyRef) {}

// CHECK: (func_decl{{.*}}"withNested(a:body:)" interface_type="(MyArray, ((@_scoped(^2.0) MyRef) -> Void) -> Void) -> ()"
func withNested(a: MyArray, body: ((@_scoped(&a) MyRef) -> Void) -> Void) {}

// An outer scope referenced from a closure type is "used" when picking the numbering for y.
// CHECK: (func_decl{{.*}}"withShared(x:body:y:)" interface_type="(@_scoped(^0.3) MyRef, (@_scoped(^0.2) MyRef, @_scoped(^1.3) MyRef) -> Void, @_scoped(^0.4) MyRef) -> ()"
func withShared(x: MyRef, body: (MyRef, @_scoped(x) MyRef) -> Void, y: MyRef) {}

// A scope bound by an inner function type doesn't affect numbering of other parameters.
// CHECK: (func_decl{{.*}}"withNestedElided(body:x:)" interface_type="(((@_scoped(^0.1) MyRef) -> Void) -> Void, @_scoped(^0.2) MyRef) -> ()"
func withNestedElided(body: ((MyRef) -> Void) -> Void, x: MyRef) {}

// MARK: - Methods

// A method's interface type is curried, `(Self) -> (Params) -> Result`, so
// `self` is one function type further out than the other parameters.
extension MyArray {
  // CHECK: (func_decl{{.*}}"ref()" interface_type="(MyArray) -> () -> @_scoped(^1.0) MyRef"
  func ref() -> @_scoped(&self) MyRef { fatalError() }

  // CHECK: (func_decl{{.*}}"mutableRef()" interface_type="(inout MyArray) -> () -> @_scoped(^1.0) MyRef"
  mutating func mutableRef() -> @_scoped(&self) MyRef { fatalError() }

  // z would be ^0.5 if y wasn't dependent on `self`.
  // CHECK: (func_decl{{.*}}"refs(x:y:z:)" interface_type="(MyArray) -> (@_scoped(^0.3) MyRef, @_scoped(^1.0) MyRef, @_scoped(^0.4) MyRef) -> ()"
  func refs(x: MyRef, y: @_scoped(&self) MyRef, z: MyRef) {}
}

// MARK: - Unsupported

// FIXME: Diagnose a scope on a type with no scope parameters instead of
// dropping it.
// CHECK: (func_decl{{.*}}"noScopeParams(a:b:)" interface_type="(@_scoped(^0.2) MyRef, MyArray) -> ()"
func noScopeParams(a: MyRef, b: @_scoped(a) MyArray) {}

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

extension MyRef {
  func nonEscapableSelf() -> @_scoped(self) MyRef { fatalError() }
  // expected-error@-1:39 {{the scope of a non-escapable 'self' is not supported}}
}

func functionTypeParam(_ body: (_ x: MyRef) -> @_scoped(x) MyRef) {}
// expected-error@-1:57 {{scopes named by function type parameters are not supported}}

// MARK: - Non-escapable parameters

func tuple(_ refs: (MyRef, Int)) {}
// expected-error@-1:20 {{non-escapable type in this position is not supported when scope restrictions are enabled}}

func optional(_ ref: MyRef?) {}
// expected-error@-1:22 {{non-escapable type in this position is not supported when scope restrictions are enabled}}

func closureResult(_ make: () -> MyRef) {}
// expected-error@-1:28 {{non-escapable type in this position is not supported when scope restrictions are enabled}}

func variadic(_ bodies: ((MyRef) -> Void)...) {}
// expected-error@-1:25 {{non-escapable type in this position is not supported when scope restrictions are enabled}}

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

// TODO: Support forward references.
func forward(b: @_scoped(a) MyRef, a: MyRef) {}
// expected-error@-1:14 {{circular reference}}
// expected-note@-2:36 {{through reference here}}
// expected-note@-3:29 {{while resolving type '@_scoped(a) MyRef'}}

// MARK: - Results

func elidedResult(_ a: MyArray, _ r: @_scoped(&a) MyRef) -> MyRef { fatalError() }
// expected-error@-1:61 {{non-escapable result of a function with scope restrictions must have its scope written with '@_scoped'}}

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

// MARK: - Pack expansions

func scopedPack<each T>(a: MyArray, _ x: repeat (each T, @_scoped(&a) MyRef)) {}
// expected-error@-1:59 {{'@_scoped' is not supported here}}

func elidedPack<each T>(_ x: repeat (each T, MyRef)) {}
// expected-error@-1:30 {{non-escapable type in this position is not supported when scope restrictions are enabled}}

// MARK: - Scopes that don't resolve

func unknown(_ a: MyArray) -> @_scoped(&b) MyRef { fatalError() }
// expected-error@-1:41 {{cannot find parameter 'b' for scope restriction}}

func noSelf() -> @_scoped(&self) MyRef { fatalError() }
// expected-error@-1:28 {{cannot find parameter 'self' for scope restriction}}

func consumed(_ a: consuming MyArray) -> @_scoped(&a) MyRef { fatalError() }
// expected-error@-1:52 {{consumed parameters have no access scope}}
// expected-note@-2:17 {{'a' is consumed}}

func owned(_ a: __owned MyArray) -> @_scoped(&a) MyRef { fatalError() }
// expected-error@-1:47 {{consumed parameters have no access scope}}
// expected-note@-2:14 {{'a' is consumed}}

func sent(_ a: sending MyArray) -> @_scoped(&a) MyRef { fatalError() }
// expected-error@-1:46 {{consumed parameters have no access scope}}
// expected-note@-2:13 {{'a' is consumed}}

func escapable(_ a: MyArray) -> @_scoped(a) MyRef { fatalError() }
// expected-error@-1:42 {{'a' has an escapable type, so it has no scope}}

func generic<T: ~Escapable>(_ t: T) -> @_scoped(t) MyRef { fatalError() }
// expected-error@-1:49 {{the scope of 't' is not supported}}

extension MyArray {
  init(a: MyArray, r: @_scoped(&a) MyRef) {}
  // expected-error@-1:33 {{consumed parameters have no access scope}}
  // expected-note@-2:8 {{initializer parameters are 'consuming' by default}}

  init(r: @_scoped(&self) MyRef) {}
  // expected-error@-1:21 {{'self' has no access scope here}}

  static func staticAccess() -> @_scoped(&self) MyRef { fatalError() }
  // expected-error@-1:43 {{'self' has no access scope here}}

  static func staticValue() -> @_scoped(self) MyRef { fatalError() }
  // expected-error@-1:41 {{'self' has an escapable type, so it has no scope}}

  consuming func consumingAccess() -> @_scoped(&self) MyRef { fatalError() }
  // expected-error@-1:49 {{consumed parameters have no access scope}}
  // expected-note@-2:18 {{'self' is consumed}}

  func escapableSelf() -> @_scoped(self) MyRef { fatalError() }
  // expected-error@-1:36 {{'self' has an escapable type, so it has no scope}}
}
