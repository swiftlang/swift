// RUN: %target-typecheck-verify-swift -enable-experimental-feature Extern -enable-experimental-feature AddressableParameters -enable-experimental-feature NoImplicitCopy -enable-experimental-feature SymbolLinkageMarkers -enable-experimental-feature StrictMemorySafety -enable-experimental-feature LifetimeDependence -enable-experimental-feature CImplementation -import-bridging-header %S/Inputs/attr_abi.h -parse-as-library -debugger-support

// REQUIRES: swift_feature_AddressableParameters
// REQUIRES: swift_feature_CImplementation
// REQUIRES: swift_feature_Extern
// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_NoImplicitCopy
// REQUIRES: swift_feature_StrictMemorySafety
// REQUIRES: swift_feature_SymbolLinkageMarkers

import _Differentiation

import Distributed

@available(SwiftStdlib 5.7, *)
typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem

//
// Same-kind checking
//

@abi(func funcForFunc_abi())
func funcForFunc() {}

@abi(var varForVar_abi: Int)
var varForVar: Int = 0

@abi(func funcForVar_abi()) // expected-error {{cannot give var 'funcForVar' the ABI of a global function}}
var funcForVar: Int = 0

@abi(var varForFunc_abi: Int) // expected-error {{cannot give global function 'varForFunc()' the ABI of a pattern binding}}
func varForFunc() {}

struct SameKind {
  @abi(subscript(sub1 _: Int) -> Int)
  subscript(sub1 _: Int) -> Int { 0 }

  @abi(func sub2(_: Int) -> Int) // expected-error {{cannot give subscript 'subscript(sub2:)' the ABI of a instance method}}
  subscript(sub2 _: Int) -> Int { 0 }

  @abi(subscript(sub3 _: Int) -> Int) // expected-error {{cannot give instance method 'sub3' the ABI of a subscript}}
  func sub3(_: Int) -> Int { 0 }

  @abi(var sub4: Int) // expected-error {{cannot give subscript 'subscript(sub4:)' the ABI of a pattern binding}}
  subscript(sub4 _: Int) -> Int { 0 }

  @abi(subscript(sub4 _: Int) -> Int) // expected-error {{cannot give property 'sub4' the ABI of a subscript}}
  var sub4: Int { 0 }
}

//
// Function arity checking
//

@abi(func param00_generic00() -> Int)
func param00_generic00() -> Int { fatalError() }

@abi(func param10_generic00(_: Int) -> Int) // expected-error {{cannot give global function 'param10_generic00()' the ABI of a global function with a different number of parameters}}
func param10_generic00() -> Int { fatalError() }

@abi(func param01_generic00() -> Int) // expected-error {{cannot give global function 'param01_generic00' the ABI of a global function with a different number of parameters}}
func param01_generic00(_: Int) -> Int { fatalError() }

@abi(func param11_generic00(_: Int) -> Int)
func param11_generic00(_: Int) -> Int { fatalError() }



@abi(func param00_generic10<T>() -> T) // expected-error {{declaration in '@abi' should not have generic signature}}
func param00_generic10() -> Int { fatalError() }

@abi(func param10_generic10<T>(_: Int) -> T) // expected-error {{declaration in '@abi' should not have generic signature}}
func param10_generic10() -> Int { fatalError() }

@abi(func param01_generic10<T>() -> T) // expected-error {{declaration in '@abi' should not have generic signature because 'param01_generic10' is not generic}}
func param01_generic10(_: Int) -> Int { fatalError() }

@abi(func param11_generic10<T>(_: Int) -> T) // expected-error {{declaration in '@abi' should not have generic signature because 'param11_generic10' is not generic}}
func param11_generic10(_: Int) -> Int { fatalError() }



@abi(func param00_generic01() -> Int) // expected-error {{declaration in '@abi' should have generic signature compatible with '<T where T : Copyable, T : Escapable>'}}
func param00_generic01<T>() -> T { fatalError() }

@abi(func param10_generic01(_: Int) -> Int) // expected-error {{declaration in '@abi' should have generic signature compatible with '<T where T : Copyable, T : Escapable>'}}
func param10_generic01<T>() -> T { fatalError() }

@abi(func param01_generic01() -> Int) // expected-error {{declaration in '@abi' should have generic signature compatible with '<T where T : Copyable, T : Escapable>'}}
func param01_generic01<T>(_: Int) -> T { fatalError() }

@abi(func param11_generic01(_: Int) -> Int) // expected-error {{declaration in '@abi' should have generic signature compatible with '<T where T : Copyable, T : Escapable>'}}
func param11_generic01<T>(_: Int) -> T { fatalError() }



@abi(func param00_generic11<T>() -> T)
func param00_generic11<T>() -> T { fatalError() }

@abi(func param10_generic11<T>(_: Int) -> T) // expected-error {{cannot give global function 'param10_generic11()' the ABI of a global function with a different number of parameters}}
func param10_generic11<T>() -> T { fatalError() }

@abi(func param01_generic11<T>() -> T) // expected-error {{cannot give global function 'param01_generic11' the ABI of a global function with a different number of parameters}}
func param01_generic11<T>(_: Int) -> T { fatalError() }

@abi(func param11_generic11<T>(_: Int) -> T)
func param11_generic11<T>(_: Int) -> T { fatalError() }



struct SubscriptArity {
  @abi(subscript(param11_generic00 _: Int) -> Int)
  subscript(param11_generic00 _: Int) -> Int { 0 }

  @abi(subscript(param21_generic00 _: Int, _: Int) -> Int) // expected-error {{cannot give subscript 'subscript(param21_generic00:)' the ABI of a subscript with a different number of parameters}}
  subscript(param21_generic00 _: Int) -> Int { 0 }

  @abi(subscript(param12_generic00 _: Int) -> Int) // expected-error {{cannot give subscript 'subscript(param12_generic00:_:)' the ABI of a subscript with a different number of parameters}}
  subscript(param12_generic00 _: Int, _: Int) -> Int { 0 }

  @abi(subscript(param22_generic00 _: Int, _: Int) -> Int)
  subscript(param22_generic00 _: Int, _: Int) -> Int { 0 }

  @abi(subscript<T>(param11_generic10 _: T) -> Int) // expected-error {{declaration in '@abi' should not have generic signature because 'subscript(param11_generic10:)' is not generic}}
  subscript(param11_generic10 _: Int) -> Int { 0 }

  @abi(subscript<T>(param21_generic10 _: T, _: Int) -> Int) // expected-error {{declaration in '@abi' should not have generic signature because 'subscript(param21_generic10:)' is not generic}}
  subscript(param21_generic10 _: Int) -> Int { 0 }

  @abi(subscript<T>(param12_generic10 _: T) -> Int) // expected-error {{declaration in '@abi' should not have generic signature because 'subscript(param12_generic10:_:)' is not generic}}
  subscript(param12_generic10 _: Int, _: Int) -> Int { 0 }

  @abi(subscript<T>(param22_generic10 _: T, _: Int) -> Int) // expected-error {{declaration in '@abi' should not have generic signature because 'subscript(param22_generic10:_:)' is not generic}}
  subscript(param22_generic10 _: Int, _: Int) -> Int { 0 }

  @abi(subscript(param11_generic01 _: Int) -> Int) // expected-error {{declaration in '@abi' should have generic signature compatible with '<T where T : Copyable, T : Escapable>'}}
  subscript<T>(param11_generic01 _: T) -> Int { 0 }

  @abi(subscript(param21_generic01 _: Int, _: Int) -> Int) // expected-error {{declaration in '@abi' should have generic signature compatible with '<T where T : Copyable, T : Escapable>'}}
  subscript<T>(param21_generic01 _: T) -> Int { 0 }

  @abi(subscript(param12_generic01 _: Int) -> Int) // expected-error {{declaration in '@abi' should have generic signature compatible with '<T where T : Copyable, T : Escapable>'}}
  subscript<T>(param12_generic01 _: T, _: Int) -> Int { 0 }

  @abi(subscript(param22_generic01 _: Int, _: Int) -> Int) // expected-error {{declaration in '@abi' should have generic signature compatible with '<T where T : Copyable, T : Escapable>'}}
  subscript<T>(param22_generic01 _: T, _: Int) -> Int { 0 }

  @abi(subscript<T>(param11_generic11 _: T) -> Int)
  subscript<T>(param11_generic11 _: T) -> Int { 0 }

  @abi(subscript<T>(param21_generic11 _: T, _: Int) -> Int) // expected-error {{cannot give subscript 'subscript(param21_generic11:)' the ABI of a subscript with a different number of parameters}}
  subscript<T>(param21_generic11 _: T) -> Int { 0 }

  @abi(subscript<T>(param12_generic11 _: T) -> Int) // expected-error {{cannot give subscript 'subscript(param12_generic11:_:)' the ABI of a subscript with a different number of parameters}}
  subscript<T>(param12_generic11 _: T, _: Int) -> Int { 0 }

  @abi(subscript<T>(param22_generic11 _: T, _: Int) -> Int)
  subscript<T>(param22_generic11 _: T, _: Int) -> Int { 0 }
}

//
// Throws effect checking
//

enum MyError: Error {}

@abi(func throws00(_: () throws -> Void))
func throws00(_: () throws -> Void) {}

@abi(func throws10(_: () throws -> Void) throws) // expected-error {{cannot give 'throws10' the ABI of a global function which can throw}}
func throws10(_: () throws -> Void) {}

@abi(func throws20(_: () throws -> Void) rethrows) // expected-error {{cannot give 'throws20' the ABI of a global function which can throw}}
func throws20(_: () throws -> Void) {}

@abi(func throws30(_: () throws -> Void) throws(MyError)) // expected-error {{cannot give 'throws30' the ABI of a global function which can throw}}
func throws30(_: () throws -> Void) {}

@abi(func throws01(_: () throws -> Void)) // expected-error {{cannot give 'throws01' the ABI of a global function which cannot throw}}
func throws01(_: () throws -> Void) throws {}

@abi(func throws11(_: () throws -> Void) throws)
func throws11(_: () throws -> Void) throws {}

@abi(func throws21(_: () throws -> Void) rethrows)
func throws21(_: () throws -> Void) throws {}

@abi(func throws31(_: () throws -> Void) throws(MyError)) // expected-error {{thrown type 'MyError' in '@abi' should match 'any Error'}}
func throws31(_: () throws -> Void) throws {} // expected-note@:37 {{should match type here}}

@abi(func throws02(_: () throws -> Void)) // expected-error {{cannot give 'throws02' the ABI of a global function which cannot throw}}
func throws02(_: () throws -> Void) rethrows {}

@abi(func throws12(_: () throws -> Void) throws)
func throws12(_: () throws -> Void) rethrows {}

@abi(func throws22(_: () throws -> Void) rethrows)
func throws22(_: () throws -> Void) rethrows {}

@abi(func throws32(_: () throws -> Void) throws(MyError)) // expected-error {{thrown type 'MyError' in '@abi' should match 'any Error'}}
func throws32(_: () throws -> Void) rethrows {} // expected-note@:37 {{should match type here}}

@abi(func throws03(_: () throws -> Void)) // expected-error {{cannot give 'throws03' the ABI of a global function which cannot throw}}
func throws03(_: () throws -> Void) throws(MyError) {}

@abi(func throws13(_: () throws -> Void) throws) // expected-error {{thrown type 'any Error' in '@abi' should match 'MyError'}}
func throws13(_: () throws -> Void) throws(MyError) {} // expected-note@:37 {{should match type here}}

@abi(func throws23(_: () throws -> Void) rethrows) // expected-error {{thrown type 'any Error' in '@abi' should match 'MyError'}}
func throws23(_: () throws -> Void) throws(MyError) {} // expected-note@:37 {{should match type here}}

@abi(func throws33(_: () throws -> Void) throws(MyError))
func throws33(_: () throws -> Void) throws(MyError) {}

@abi(var throws00Var: Int)
var throws00Var: Int { get { fatalError() } }

@abi(var throws11Var: Int)
var throws11Var: Int { get throws { fatalError() } }

enum ErsatzResult<Success, Failure: Error> {}

extension ErsatzResult where Failure == Swift.Error {
  // The `where` clause makes `throws(Failure)` equivalent to `throws`.

  // Similar to Swift.Result.init(__untyped_throws_catching:)
  @abi(
    init(
      catching body: () throws -> Success
    )
  )
  init(
    __untyped_throws_catching body: () throws(Failure) -> Success
  ) {}

  @abi(func get() throws -> Success)
  func __untyped_throws_get() throws(Failure) -> Success { fatalError() }
}

extension ErsatzResult {
  // Should not be allowed, as `Failure` is still generic
  @abi(
    init(
      unconstrainedCatching body: () throws -> Success // expected-error {{parameter 'body' type '() throws -> Success' in '@abi' should match '() throws(Failure) -> Success'}}
    )
  )
  init(
    __untyped_throws_catching_bad body: () throws(Failure) -> Success // expected-note {{should match type here}}
  ) {}

  @abi(func unconstrainedGet() throws -> Success) // expected-error @:32 {{thrown type 'any Error' in '@abi' should match 'Failure'}}
  func __untyped_throws_get_bad() throws(Failure) -> Success { fatalError() } // expected-note {{should match type here}}
}

//
// Async effect checking
//

@abi(func async00())
func async00() {}

@abi(func async10() async) // expected-error {{cannot give 'async10()' the ABI of an async global function}}
func async10() {}

@abi(func async01()) // expected-error {{cannot give 'async01()' the ABI of a non-async global function}}
func async01() async {}

@abi(func async11() async)
func async11() async {}

@abi(var async00Var: Int)
var async00Var: Int { get { fatalError() } }

@abi(var async11Var: Int)
var async11Var: Int { get async { fatalError() } }

//
// PBD shape checking
//

@abi(var x1, y1: Int)
var x1: Int = 0 // expected-error {{'abi' attribute can only be applied to a single var; declare each var separately}}

@abi(var x2: Int)
var x2 = 0, y2: Int = 0 // expected-error {{'abi' attribute can only be applied to a single var; declare each var separately}}

@abi(var (x3, y3): (Int, Int), (a3, b3): (Int, Int))
var (x3, y3): (Int, Int) = (0, 0), a3: Int = 0 // expected-error {{'abi' attribute can only be applied to a single var; declare each var separately}}

@abi(var (x4, y4): (Int, Int), a4: Int)
var (x4, y4): (Int, Int) = (0, 0), (a4, b4): (Int, Int) = (0, 0) // expected-error {{'abi' attribute can only be applied to a single var; declare each var separately}}

@abi(var x5: Int)
var x5: Int = 0

//
// Redeclaration diagnostics
//

@abi(func noConflictWithSelf())
func noConflictWithSelf() {}

@abi(func noConflictWithMutualChanges1())
func noConflictWithMutualChanges2() {}

@abi(func noConflictWithMutualChanges2())
func noConflictWithMutualChanges1() {}

@abi(func conflictsWithOtherABI()) // expected-note {{'conflictsWithOtherABI()' previously declared here}}
func conflictsWithOtherABI1() {}

@abi(func conflictsWithOtherABI()) // expected-error {{invalid redeclaration of 'conflictsWithOtherABI()'}}
func conflictsWithOtherABI2() {}

@abi(func conflictsWithNonABI()) // expected-error {{invalid redeclaration of 'conflictsWithNonABI()'}}
func conflictsWithNonABI_formal() {}

func conflictsWithNonABI() {} // expected-note {{'conflictsWithNonABI()' previously declared here}}

@abi(var var_noConflictWithSelf: Int)
var var_noConflictWithSelf: Int = 0

@abi(var var_noConflictWithMutualChanges1: Int)
var var_noConflictWithMutualChanges2: Int = 0

@abi(var var_noConflictWithMutualChanges2: Int)
var var_noConflictWithMutualChanges1: Int = 0

@abi(var var_conflictsWithOtherABI: Int) // expected-note {{'var_conflictsWithOtherABI' previously declared here}}
var var_conflictsWithOtherABI1: Int = 0

@abi(var var_conflictsWithOtherABI: Int) // expected-error {{invalid redeclaration of 'var_conflictsWithOtherABI'}}
var var_conflictsWithOtherABI2: Int = 0

@abi(var var_conflictsWithNonABI: Int) // expected-error {{invalid redeclaration of 'var_conflictsWithNonABI'}}
var var_conflictsWithNonABI_formal: Int = 0

var var_conflictsWithNonABI: Int = 0 // expected-note {{'var_conflictsWithNonABI' previously declared here}}

struct Foo {
  @abi(func noConflictWithSelf())
  func noConflictWithSelf() {}

  @abi(func noConflictWithMutualChanges1())
  func noConflictWithMutualChanges2() {}

  @abi(func noConflictWithMutualChanges2())
  func noConflictWithMutualChanges1() {}

  @abi(func conflictsWithOtherABI()) // expected-note {{'conflictsWithOtherABI()' previously declared here}}
  func conflictsWithOtherABI1() {}

  @abi(func conflictsWithOtherABI()) // expected-error {{invalid redeclaration of 'conflictsWithOtherABI()'}}
  func conflictsWithOtherABI2() {}

  @abi(func conflictsWithNonABI()) // expected-error {{invalid redeclaration of 'conflictsWithNonABI()'}}
  func conflictsWithNonABI_formal() {}

  func conflictsWithNonABI() {} // expected-note {{'conflictsWithNonABI()' previously declared here}}

  @abi(var var_noConflictWithSelf: Int)
  var var_noConflictWithSelf: Int = 0

  @abi(var var_noConflictWithMutualChanges1: Int)
  var var_noConflictWithMutualChanges2: Int = 0

  @abi(var var_noConflictWithMutualChanges2: Int)
  var var_noConflictWithMutualChanges1: Int = 0

  @abi(var var_conflictsWithOtherABI: Int) // expected-note {{'var_conflictsWithOtherABI' previously declared here}}
  var var_conflictsWithOtherABI1: Int = 0

  @abi(var var_conflictsWithOtherABI: Int) // expected-error {{invalid redeclaration of 'var_conflictsWithOtherABI'}}
  var var_conflictsWithOtherABI2: Int = 0

  @abi(var var_conflictsWithNonABI: Int) // expected-error {{invalid redeclaration of 'var_conflictsWithNonABI'}}
  var var_conflictsWithNonABI_formal: Int = 0

  var var_conflictsWithNonABI: Int = 0 // expected-note {{'var_conflictsWithNonABI' previously declared here}}
}

func fn() {
  // TODO: Figure out if @abi makes sense in local scope and, if so, when we
  //       should complain about redecls.

  @abi(func noConflictWithSelf())
  func noConflictWithSelf() {}

  @abi(func noConflictWithMutualChanges1())
  func noConflictWithMutualChanges2() {}

  @abi(func noConflictWithMutualChanges2())
  func noConflictWithMutualChanges1() {}

  @abi(func conflictsWithOtherABI()) // expected-note {{'conflictsWithOtherABI()' previously declared here}}
  func conflictsWithOtherABI1() {}

  @abi(func conflictsWithOtherABI()) // expected-error {{invalid redeclaration of 'conflictsWithOtherABI()'}}
  func conflictsWithOtherABI2() {}

  @abi(func conflictsWithNonABI()) // expected-error {{invalid redeclaration of 'conflictsWithNonABI()'}}
  func conflictsWithNonABI_formal() {}

  func conflictsWithNonABI() {} // expected-note {{'conflictsWithNonABI()' previously declared here}}

  var a = 1 // expected-note {{'a' previously declared here}}
  var a = 1 // expected-error {{invalid redeclaration of 'a'}}

  @abi(var var_noConflictWithSelf: Int)
  var var_noConflictWithSelf: Int = 0

  @abi(var var_noConflictWithMutualChanges1: Int)
  var var_noConflictWithMutualChanges2: Int = 0

  @abi(var var_noConflictWithMutualChanges2: Int)
  var var_noConflictWithMutualChanges1: Int = 0

  @abi(var var_conflictsWithOtherABI: Int) // expected-note {{'var_conflictsWithOtherABI' previously declared here}}
  var var_conflictsWithOtherABI1: Int = 0

  @abi(var var_conflictsWithOtherABI: Int) // expected-error {{invalid redeclaration of 'var_conflictsWithOtherABI'}}
  var var_conflictsWithOtherABI2: Int = 0

  // Diagnosed in the opposite order from usual due to being in a local scope where visibility may be limited:
  @abi(var var_conflictsWithNonABI: Int) // expected-note {{'var_conflictsWithNonABI' previously declared here}}
  var var_conflictsWithNonABI_formal: Int = 0

  var var_conflictsWithNonABI: Int = 0 // expected-error {{invalid redeclaration of 'var_conflictsWithNonABI'}}
}

//
// Type differences
//

@abi(func floatForIntParam(_ a: Float) -> Int) // expected-error @:33 {{parameter 'a' type 'Float' in '@abi' should match 'Int'}}
func intForFloatParam(_: Int) -> Int { fatalError() } // expected-note @:26 {{should match type here}}

@abi(func floatForIntResult(_ a: Int) -> Float) // expected-error @:42 {{result type 'Float' in '@abi' should match 'Int'}}
func intForFloatResult(_: Int) -> Int { fatalError() } // expected-note @:35 {{should match type here}}

@abi(func labeledForUnlabeledTuple(_: (x: Int, y: Int)))
func labeledForUnlabeledTuple(_: (Int, Int)) {}

@abi(func unlabeledForLabeledTuple(_: (Int, Int)))
func unlabeledForLabeledTuple(_: (x: Int, y: Int)) {}

@abi(func labeledForLabeledTuple(_: (x: Int, y: Int)))
func labeledForLabeledTuple(_: (a: Int, b: Int)) {}

@abi(
  func testDefaultArguments(
    a: Int,
    b: Int = 1, // expected-error {{'b' in '@abi' should not have a default argument; it does not affect the parameter's ABI}}
    c: Int,
    d: Int = 2 // expected-error {{'d' in '@abi' should not have a default argument; it does not affect the parameter's ABI}}
  )
)
func testDefaultArguments(
  a: Int,
  b: Int,
  c: Int = 1,
  d: Int = 2
) {}

@abi(func arrayForVariadicParam(a: [Int], b: Set<Float>)) // expected-error @:46 {{parameter 'b' type 'Set<Float>' in '@abi' should match 'Float...'}}
func arrayForVariadicParam(a: Int..., b: Float...) {} // expected-note @:42 {{should match type here}}

struct DefaultParamOwnership {
  @abi(
    func method(
      _ a: AnyObject,
      _ b: inout AnyObject, // expected-error {{modifier 'inout' on parameter 'b' in '@abi' is not compatible with default}}
      _ c: borrowing AnyObject,
      _ d: consuming AnyObject, // expected-error {{modifier 'consuming' on parameter 'd' in '@abi' is not compatible with default}}
      _ e: __shared AnyObject,
      _ f: __owned AnyObject, // expected-error {{modifier '__owned' on parameter 'f' in '@abi' is not compatible with default}}
      _ g: sending AnyObject, // expected-error {{modifier 'sending' on parameter 'g' in '@abi' is not compatible with default}}
      _ h: (AnyObject) -> Void,
      _ i: (borrowing AnyObject) -> Void,
      _ j: (consuming AnyObject) -> Void // expected-error {{parameter 'j' type '(consuming AnyObject) -> Void' in '@abi' should match '(AnyObject) -> Void'}}
    )
  )
  func method(
    _: AnyObject,
    _: AnyObject, // expected-note {{should match type here}}
    _: AnyObject,
    _: AnyObject, // expected-note {{should match type here}}
    _: AnyObject,
    _: AnyObject, // expected-note {{should match type here}}
    _: AnyObject, // expected-note {{should match type here}}
    _: (AnyObject) -> Void,
    _: (AnyObject) -> Void,
    _: (AnyObject) -> Void // expected-note {{should match type here}}
  ) {}

  @abi(
    init(
      _ a: AnyObject,
      _ b: inout AnyObject, // expected-error {{modifier 'inout' on parameter 'b' in '@abi' is not compatible with default}}
      _ c: borrowing AnyObject, // expected-error {{modifier 'borrowing' on parameter 'c' in '@abi' is not compatible with default}}
      _ d: consuming AnyObject,
      _ e: __shared AnyObject, // expected-error {{modifier '__shared' on parameter 'e' in '@abi' is not compatible with default}}
      _ f: __owned AnyObject,
      _ g: sending AnyObject,
      _ h: (AnyObject) -> Void,
      _ i: (borrowing AnyObject) -> Void,
      _ j: (consuming AnyObject) -> Void // expected-error {{parameter 'j' type '(consuming AnyObject) -> Void' in '@abi' should match '(AnyObject) -> Void'}}
    )
  )
  init(
    _: AnyObject,
    _: AnyObject, // expected-note {{should match type here}}
    _: AnyObject, // expected-note {{should match type here}}
    _: AnyObject,
    _: AnyObject, // expected-note {{should match type here}}
    _: AnyObject,
    _: AnyObject,
    _: (AnyObject) -> Void,
    _: (AnyObject) -> Void,
    _: (AnyObject) -> Void // expected-note {{should match type here}}
  ) {}
}

struct InoutParamOwnership {
  @abi(
    func method(
      _ a: AnyObject, // expected-error {{default modifier on parameter 'a' in '@abi' is not compatible with 'inout'}}
      _ b: inout AnyObject,
      _ c: borrowing AnyObject, // expected-error {{modifier 'borrowing' on parameter 'c' in '@abi' is not compatible with 'inout'}}
      _ d: consuming AnyObject, // expected-error {{modifier 'consuming' on parameter 'd' in '@abi' is not compatible with 'inout'}}
      _ e: __shared AnyObject, // expected-error {{modifier '__shared' on parameter 'e' in '@abi' is not compatible with 'inout'}}
      _ f: __owned AnyObject, // expected-error {{modifier '__owned' on parameter 'f' in '@abi' is not compatible with 'inout'}}
      _ g: sending AnyObject, // expected-error {{modifier 'sending' on parameter 'g' in '@abi' is not compatible with 'inout'}}
      _ h: (AnyObject) -> Void, // expected-error {{parameter 'h' type '(AnyObject) -> Void' in '@abi' should match '(inout AnyObject) -> Void'}}
      _ i: (borrowing AnyObject) -> Void, // expected-error {{parameter 'i' type '(borrowing AnyObject) -> Void' in '@abi' should match '(inout AnyObject) -> Void'}}
      _ j: (consuming AnyObject) -> Void // expected-error {{parameter 'j' type '(consuming AnyObject) -> Void' in '@abi' should match '(inout AnyObject) -> Void'}}
    )
  )
  func method(
    _: inout AnyObject, // expected-note {{should match type here}}
    _: inout AnyObject,
    _: inout AnyObject, // expected-note {{should match type here}}
    _: inout AnyObject, // expected-note {{should match type here}}
    _: inout AnyObject, // expected-note {{should match type here}}
    _: inout AnyObject, // expected-note {{should match type here}}
    _: inout AnyObject, // expected-note {{should match type here}}
    _: (inout AnyObject) -> Void, // expected-note {{should match type here}}
    _: (inout AnyObject) -> Void, // expected-note {{should match type here}}
    _: (inout AnyObject) -> Void // expected-note {{should match type here}}
  ) {}

  @abi(
    init(
      _ a: AnyObject, // expected-error {{default modifier on parameter 'a' in '@abi' is not compatible with 'inout'}}
      _ b: inout AnyObject,
      _ c: borrowing AnyObject, // expected-error {{modifier 'borrowing' on parameter 'c' in '@abi' is not compatible with 'inout'}}
      _ d: consuming AnyObject, // expected-error {{modifier 'consuming' on parameter 'd' in '@abi' is not compatible with 'inout'}}
      _ e: __shared AnyObject, // expected-error {{modifier '__shared' on parameter 'e' in '@abi' is not compatible with 'inout'}}
      _ f: __owned AnyObject, // expected-error {{modifier '__owned' on parameter 'f' in '@abi' is not compatible with 'inout'}}
      _ g: sending AnyObject, // expected-error {{modifier 'sending' on parameter 'g' in '@abi' is not compatible with 'inout'}}
      _ h: (AnyObject) -> Void, // expected-error {{parameter 'h' type '(AnyObject) -> Void' in '@abi' should match '(inout AnyObject) -> Void'}}
      _ i: (borrowing AnyObject) -> Void, // expected-error {{parameter 'i' type '(borrowing AnyObject) -> Void' in '@abi' should match '(inout AnyObject) -> Void'}}
      _ j: (consuming AnyObject) -> Void // expected-error {{parameter 'j' type '(consuming AnyObject) -> Void' in '@abi' should match '(inout AnyObject) -> Void'}}
    )
  )
  init(
    _: inout AnyObject, // expected-note {{should match type here}}
    _: inout AnyObject,
    _: inout AnyObject, // expected-note {{should match type here}}
    _: inout AnyObject, // expected-note {{should match type here}}
    _: inout AnyObject, // expected-note {{should match type here}}
    _: inout AnyObject, // expected-note {{should match type here}}
    _: inout AnyObject, // expected-note {{should match type here}}
    _: (inout AnyObject) -> Void, // expected-note {{should match type here}}
    _: (inout AnyObject) -> Void, // expected-note {{should match type here}}
    _: (inout AnyObject) -> Void // expected-note {{should match type here}}
  ) {}
}

struct BorrowingParamOwnership {
  @abi(
    func method(
      _ a: AnyObject,
      _ b: inout AnyObject, // expected-error {{modifier 'inout' on parameter 'b' in '@abi' is not compatible with 'borrowing'}}
      _ c: borrowing AnyObject,
      _ d: consuming AnyObject, // expected-error {{modifier 'consuming' on parameter 'd' in '@abi' is not compatible with 'borrowing'}}
      _ e: __shared AnyObject,
      _ f: __owned AnyObject, // expected-error {{modifier '__owned' on parameter 'f' in '@abi' is not compatible with 'borrowing'}}
      _ g: sending AnyObject, // expected-error {{modifier 'sending' on parameter 'g' in '@abi' is not compatible with 'borrowing'}}
      _ h: (AnyObject) -> Void,
      _ i: (borrowing AnyObject) -> Void,
      _ j: (consuming AnyObject) -> Void // expected-error {{parameter 'j' type '(consuming AnyObject) -> Void' in '@abi' should match '(borrowing AnyObject) -> Void'}}
    )
  )
  func method(
    _: borrowing AnyObject,
    _: borrowing AnyObject, // expected-note {{should match type here}}
    _: borrowing AnyObject,
    _: borrowing AnyObject, // expected-note {{should match type here}}
    _: borrowing AnyObject,
    _: borrowing AnyObject, // expected-note {{should match type here}}
    _: borrowing AnyObject, // expected-note {{should match type here}}
    _: (borrowing AnyObject) -> Void,
    _: (borrowing AnyObject) -> Void,
    _: (borrowing AnyObject) -> Void // expected-note {{should match type here}}
  ) {}

  @abi(
    init(
      _ a: AnyObject, // expected-error {{default modifier on parameter 'a' in '@abi' is not compatible with 'borrowing'}}
      _ b: inout AnyObject, // expected-error {{modifier 'inout' on parameter 'b' in '@abi' is not compatible with 'borrowing'}}
      _ c: borrowing AnyObject,
      _ d: consuming AnyObject, // expected-error {{modifier 'consuming' on parameter 'd' in '@abi' is not compatible with 'borrowing'}}
      _ e: __shared AnyObject,
      _ f: __owned AnyObject, // expected-error {{modifier '__owned' on parameter 'f' in '@abi' is not compatible with 'borrowing'}}
      _ g: sending AnyObject, // expected-error {{modifier 'sending' on parameter 'g' in '@abi' is not compatible with 'borrowing'}}
      _ h: (AnyObject) -> Void,
      _ i: (borrowing AnyObject) -> Void,
      _ j: (consuming AnyObject) -> Void // expected-error {{parameter 'j' type '(consuming AnyObject) -> Void' in '@abi' should match '(borrowing AnyObject) -> Void'}}
    )
  )
  init(
    _: borrowing AnyObject, // expected-note {{should match type here}}
    _: borrowing AnyObject, // expected-note {{should match type here}}
    _: borrowing AnyObject,
    _: borrowing AnyObject, // expected-note {{should match type here}}
    _: borrowing AnyObject,
    _: borrowing AnyObject, // expected-note {{should match type here}}
    _: borrowing AnyObject, // expected-note {{should match type here}}
    _: (borrowing AnyObject) -> Void,
    _: (borrowing AnyObject) -> Void,
    _: (borrowing AnyObject) -> Void // expected-note {{should match type here}}
  ) {}
}

struct ConsumingParamOwnership {
  @abi(
    func method(
      _ a: AnyObject, // expected-error {{default modifier on parameter 'a' in '@abi' is not compatible with 'consuming'}}
      _ b: inout AnyObject, // expected-error {{modifier 'inout' on parameter 'b' in '@abi' is not compatible with 'consuming'}}
      _ c: borrowing AnyObject, // expected-error {{modifier 'borrowing' on parameter 'c' in '@abi' is not compatible with 'consuming'}}
      _ d: consuming AnyObject,
      _ e: __shared AnyObject, // expected-error {{modifier '__shared' on parameter 'e' in '@abi' is not compatible with 'consuming'}}
      _ f: __owned AnyObject,
      _ g: sending AnyObject,
      _ h: (AnyObject) -> Void, // expected-error {{parameter 'h' type '(AnyObject) -> Void' in '@abi' should match '(consuming AnyObject) -> Void'}}
      _ i: (borrowing AnyObject) -> Void, // expected-error {{parameter 'i' type '(borrowing AnyObject) -> Void' in '@abi' should match '(consuming AnyObject) -> Void'}}
      _ j: (consuming AnyObject) -> Void
    )
  )
  func method(
    _: consuming AnyObject, // expected-note {{should match type here}}
    _: consuming AnyObject, // expected-note {{should match type here}}
    _: consuming AnyObject, // expected-note {{should match type here}}
    _: consuming AnyObject,
    _: consuming AnyObject, // expected-note {{should match type here}}
    _: consuming AnyObject,
    _: consuming AnyObject,
    _: (consuming AnyObject) -> Void, // expected-note {{should match type here}}
    _: (consuming AnyObject) -> Void, // expected-note {{should match type here}}
    _: (consuming AnyObject) -> Void
  ) {}

  @abi(
    init(
      _ a: AnyObject,
      _ b: inout AnyObject, // expected-error {{modifier 'inout' on parameter 'b' in '@abi' is not compatible with 'consuming'}}
      _ c: borrowing AnyObject, // expected-error {{modifier 'borrowing' on parameter 'c' in '@abi' is not compatible with 'consuming'}}
      _ d: consuming AnyObject,
      _ e: __shared AnyObject, // expected-error {{modifier '__shared' on parameter 'e' in '@abi' is not compatible with 'consuming'}}
      _ f: __owned AnyObject,
      _ g: sending AnyObject,
      _ h: (AnyObject) -> Void, // expected-error {{parameter 'h' type '(AnyObject) -> Void' in '@abi' should match '(consuming AnyObject) -> Void'}}
      _ i: (borrowing AnyObject) -> Void, // expected-error {{parameter 'i' type '(borrowing AnyObject) -> Void' in '@abi' should match '(consuming AnyObject) -> Void'}}
      _ j: (consuming AnyObject) -> Void
    )
  )
  init(
    _: consuming AnyObject,
    _: consuming AnyObject, // expected-note {{should match type here}}
    _: consuming AnyObject, // expected-note {{should match type here}}
    _: consuming AnyObject,
    _: consuming AnyObject, // expected-note {{should match type here}}
    _: consuming AnyObject,
    _: consuming AnyObject,
    _: (consuming AnyObject) -> Void, // expected-note {{should match type here}}
    _: (consuming AnyObject) -> Void, // expected-note {{should match type here}}
    _: (consuming AnyObject) -> Void
  ) {}
}

struct SharedParamOwnership {
  @abi(
    func method(
      _ a: AnyObject,
      _ b: inout AnyObject, // expected-error {{modifier 'inout' on parameter 'b' in '@abi' is not compatible with '__shared'}}
      _ c: borrowing AnyObject,
      _ d: consuming AnyObject, // expected-error {{modifier 'consuming' on parameter 'd' in '@abi' is not compatible with '__shared'}}
      _ e: __shared AnyObject,
      _ f: __owned AnyObject, // expected-error {{modifier '__owned' on parameter 'f' in '@abi' is not compatible with '__shared'}}
      _ g: sending AnyObject, // expected-error {{modifier 'sending' on parameter 'g' in '@abi' is not compatible with '__shared'}}
      _ h: (AnyObject) -> Void,
      _ i: (borrowing AnyObject) -> Void,
      _ j: (consuming AnyObject) -> Void // expected-error {{parameter 'j' type '(consuming AnyObject) -> Void' in '@abi' should match '(__shared AnyObject) -> Void'}}
    )
  )
  func method(
    _: __shared AnyObject,
    _: __shared AnyObject, // expected-note {{should match type here}}
    _: __shared AnyObject,
    _: __shared AnyObject, // expected-note {{should match type here}}
    _: __shared AnyObject,
    _: __shared AnyObject, // expected-note {{should match type here}}
    _: __shared AnyObject, // expected-note {{should match type here}}
    _: (__shared AnyObject) -> Void,
    _: (__shared AnyObject) -> Void,
    _: (__shared AnyObject) -> Void // expected-note {{should match type here}}
  ) {}

  @abi(
    init(
      _ a: AnyObject, // expected-error {{default modifier on parameter 'a' in '@abi' is not compatible with '__shared'}}
      _ b: inout AnyObject, // expected-error {{modifier 'inout' on parameter 'b' in '@abi' is not compatible with '__shared'}}
      _ c: borrowing AnyObject,
      _ d: consuming AnyObject, // expected-error {{modifier 'consuming' on parameter 'd' in '@abi' is not compatible with '__shared'}}
      _ e: __shared AnyObject,
      _ f: __owned AnyObject, // expected-error {{modifier '__owned' on parameter 'f' in '@abi' is not compatible with '__shared'}}
      _ g: sending AnyObject, // expected-error {{modifier 'sending' on parameter 'g' in '@abi' is not compatible with '__shared'}}
      _ h: (AnyObject) -> Void,
      _ i: (borrowing AnyObject) -> Void,
      _ j: (consuming AnyObject) -> Void // expected-error {{parameter 'j' type '(consuming AnyObject) -> Void' in '@abi' should match '(__shared AnyObject) -> Void'}}
    )
  )
  init(
    _: __shared AnyObject, // expected-note {{should match type here}}
    _: __shared AnyObject, // expected-note {{should match type here}}
    _: __shared AnyObject,
    _: __shared AnyObject, // expected-note {{should match type here}}
    _: __shared AnyObject,
    _: __shared AnyObject, // expected-note {{should match type here}}
    _: __shared AnyObject, // expected-note {{should match type here}}
    _: (__shared AnyObject) -> Void,
    _: (__shared AnyObject) -> Void,
    _: (__shared AnyObject) -> Void // expected-note {{should match type here}}
  ) {}
}

struct OwnedParamOwnership {
  @abi(
    func method(
      _ a: AnyObject, // expected-error {{default modifier on parameter 'a' in '@abi' is not compatible with '__owned'}}
      _ b: inout AnyObject, // expected-error {{modifier 'inout' on parameter 'b' in '@abi' is not compatible with '__owned'}}
      _ c: borrowing AnyObject, // expected-error {{modifier 'borrowing' on parameter 'c' in '@abi' is not compatible with '__owned'}}
      _ d: consuming AnyObject,
      _ e: __shared AnyObject, // expected-error {{modifier '__shared' on parameter 'e' in '@abi' is not compatible with '__owned'}}
      _ f: __owned AnyObject,
      _ g: sending AnyObject,
      _ h: (AnyObject) -> Void, // expected-error {{parameter 'h' type '(AnyObject) -> Void' in '@abi' should match '(__owned AnyObject) -> Void'}}
      _ i: (borrowing AnyObject) -> Void, // expected-error {{parameter 'i' type '(borrowing AnyObject) -> Void' in '@abi' should match '(__owned AnyObject) -> Void'}}
      _ j: (consuming AnyObject) -> Void
    )
  )
  func method(
    _: __owned AnyObject, // expected-note {{should match type here}}
    _: __owned AnyObject, // expected-note {{should match type here}}
    _: __owned AnyObject, // expected-note {{should match type here}}
    _: __owned AnyObject,
    _: __owned AnyObject, // expected-note {{should match type here}}
    _: __owned AnyObject,
    _: __owned AnyObject,
    _: (__owned AnyObject) -> Void, // expected-note {{should match type here}}
    _: (__owned AnyObject) -> Void, // expected-note {{should match type here}}
    _: (__owned AnyObject) -> Void
  ) {}

  @abi(
    init(
      _ a: AnyObject,
      _ b: inout AnyObject, // expected-error {{modifier 'inout' on parameter 'b' in '@abi' is not compatible with '__owned'}}
      _ c: borrowing AnyObject, // expected-error {{modifier 'borrowing' on parameter 'c' in '@abi' is not compatible with '__owned'}}
      _ d: consuming AnyObject,
      _ e: __shared AnyObject, // expected-error {{modifier '__shared' on parameter 'e' in '@abi' is not compatible with '__owned'}}
      _ f: __owned AnyObject,
      _ g: sending AnyObject,
      _ h: (AnyObject) -> Void, // expected-error {{parameter 'h' type '(AnyObject) -> Void' in '@abi' should match '(__owned AnyObject) -> Void'}}
      _ i: (borrowing AnyObject) -> Void, // expected-error {{parameter 'i' type '(borrowing AnyObject) -> Void' in '@abi' should match '(__owned AnyObject) -> Void'}}
      _ j: (consuming AnyObject) -> Void
    )
  )
  init(
    _: __owned AnyObject,
    _: __owned AnyObject, // expected-note {{should match type here}}
    _: __owned AnyObject, // expected-note {{should match type here}}
    _: __owned AnyObject,
    _: __owned AnyObject, // expected-note {{should match type here}}
    _: __owned AnyObject,
    _: __owned AnyObject,
    _: (__owned AnyObject) -> Void, // expected-note {{should match type here}}
    _: (__owned AnyObject) -> Void, // expected-note {{should match type here}}
    _: (__owned AnyObject) -> Void
  ) {}
}

struct SendingParamOwnership {
  @abi(
    func method(
      _ a: AnyObject, // expected-error {{default modifier on parameter 'a' in '@abi' is not compatible with 'sending'}}
      _ b: inout AnyObject, // expected-error {{modifier 'inout' on parameter 'b' in '@abi' is not compatible with 'sending'}}
      _ c: borrowing AnyObject, // expected-error {{modifier 'borrowing' on parameter 'c' in '@abi' is not compatible with 'sending'}}
      _ d: consuming AnyObject,
      _ e: __shared AnyObject, // expected-error {{modifier '__shared' on parameter 'e' in '@abi' is not compatible with 'sending'}}
      _ f: __owned AnyObject,
      _ g: sending AnyObject,
      _ h: (AnyObject) -> Void, // expected-error {{parameter 'h' type '(AnyObject) -> Void' in '@abi' should match '(sending AnyObject) -> Void'}}
      _ i: (borrowing AnyObject) -> Void, // expected-error {{parameter 'i' type '(borrowing AnyObject) -> Void' in '@abi' should match '(sending AnyObject) -> Void'}}
      _ j: (consuming AnyObject) -> Void
    )
  )
  func method(
    _: sending AnyObject, // expected-note {{should match type here}}
    _: sending AnyObject, // expected-note {{should match type here}}
    _: sending AnyObject, // expected-note {{should match type here}}
    _: sending AnyObject,
    _: sending AnyObject, // expected-note {{should match type here}}
    _: sending AnyObject,
    _: sending AnyObject,
    _: (sending AnyObject) -> Void, // expected-note {{should match type here}}
    _: (sending AnyObject) -> Void, // expected-note {{should match type here}}
    _: (sending AnyObject) -> Void
  ) {}

  @abi(
    init(
      _ a: AnyObject,
      _ b: inout AnyObject, // expected-error {{modifier 'inout' on parameter 'b' in '@abi' is not compatible with 'sending'}}
      _ c: borrowing AnyObject, // expected-error {{modifier 'borrowing' on parameter 'c' in '@abi' is not compatible with 'sending'}}
      _ d: consuming AnyObject,
      _ e: __shared AnyObject, // expected-error {{modifier '__shared' on parameter 'e' in '@abi' is not compatible with 'sending'}}
      _ f: __owned AnyObject,
      _ g: sending AnyObject,
      _ h: (AnyObject) -> Void, // expected-error {{parameter 'h' type '(AnyObject) -> Void' in '@abi' should match '(sending AnyObject) -> Void'}}
      _ i: (borrowing AnyObject) -> Void, // expected-error {{parameter 'i' type '(borrowing AnyObject) -> Void' in '@abi' should match '(sending AnyObject) -> Void'}}
      _ j: (consuming AnyObject) -> Void
    )
  )
  init(
    _: sending AnyObject,
    _: sending AnyObject, // expected-note {{should match type here}}
    _: sending AnyObject, // expected-note {{should match type here}}
    _: sending AnyObject,
    _: sending AnyObject, // expected-note {{should match type here}}
    _: sending AnyObject,
    _: sending AnyObject,
    _: (sending AnyObject) -> Void, // expected-note {{should match type here}}
    _: (sending AnyObject) -> Void, // expected-note {{should match type here}}
    _: (sending AnyObject) -> Void
  ) {}
}

// @autoclosure should be flattened away
@abi(
  func autoclosureTest(
    _: @autoclosure () -> Void,
    _: () -> Void,
    _: @autoclosure () -> Void,
    _: () -> Void
  )
)
func autoclosureTest(
  _: @autoclosure () -> Void,
  _: @autoclosure () -> Void,
  _: () -> Void,
  _: () -> Void
) {}

// @_nonEphemeral should be flattened away
// (the diagnostic we get on these is actually part of attr checking)
@abi(
  func nonEphemeralTest(
    @_nonEphemeral _: UnsafeRawPointer, // expected-error {{unused '_nonEphemeral' attribute in '@abi'}}
    _: UnsafeRawPointer,
    @_nonEphemeral _: UnsafeRawPointer, // expected-error {{unused '_nonEphemeral' attribute in '@abi'}}
    _: UnsafeRawPointer
  )
)
func nonEphemeralTest(
  @_nonEphemeral _: UnsafeRawPointer,
  @_nonEphemeral _: UnsafeRawPointer,
  _: UnsafeRawPointer,
  _: UnsafeRawPointer
) {}

// isolated param should be flattened away
@abi(func isolatedTestMismatch<A: Actor>(_: isolated A, _: A))
func isolatedTestMismatch<A: Actor>(_: A, _: isolated A) {}

@abi(func isolatedTestMatch<A: Actor>(_: isolated A, _: A))
func isolatedTestMatch<A: Actor>(_: isolated A, _: A) {}

// _const should be flattened away
@abi(
  func constTest(
    _: _const () -> Void,
    _: () -> Void,
    _: _const () -> Void,
    _: () -> Void
  )
)
func constTest(
  _: _const () -> Void,
  _: _const () -> Void,
  _: () -> Void,
  _: () -> Void
) {}

// @noDerivative should match
@abi(func noDerivativeTest1(_ a: @differentiable(reverse) (@noDerivative Double, Double) -> Double))
func noDerivativeTest1(_: @differentiable(reverse) (@noDerivative Double, Double) -> Double) {}

@abi(func noDerivativeTest2(_ a: @differentiable(reverse) (Double, Double) -> Double)) // expected-error {{parameter 'a' type '@differentiable(reverse) (Double, Double) -> Double' in '@abi' should match '@differentiable(reverse) (@noDerivative Double, Double) -> Double'}}
func noDerivativeTest2(_: @differentiable(reverse) (@noDerivative Double, Double) -> Double) {} // expected-note {{should match type here}}

@abi(func noDerivativeTest3(_ a: @differentiable(reverse) (@noDerivative Double, Double) -> Double)) // expected-error {{parameter 'a' type '@differentiable(reverse) (@noDerivative Double, Double) -> Double' in '@abi' should match '@differentiable(reverse) (Double, Double) -> Double'}}
func noDerivativeTest3(_: @differentiable(reverse) (Double, Double) -> Double) {} // expected-note {{should match type here}}

// @_addressable should match
@abi(
  func addressableTest(
    _ a: @_addressable String,
    _ b: String, // expected-error {{default attribute on parameter 'b' in '@abi' is not compatible with '_addressable'}}
    _ c: @_addressable String, // expected-error {{attribute '_addressable' on parameter 'c' in '@abi' is not compatible with default}}
    _ d: String
  )
)
func addressableTest(
  _: @_addressable String,
  _: @_addressable String, // expected-note {{should match type here}}
  _: String, // expected-note {{should match type here}}
  _: String
) {}

// Flattening of function type ExtInfo
@abi(
  func fnExtInfoTest(
    _ a: @escaping () -> AnyObject,
    _ b: @Sendable () -> AnyObject,
    _ c: () -> sending AnyObject,
    _ d: () -> AnyObject,
    _ e: @MainActor () -> AnyObject,
    _ f: (isolated MainActor) -> AnyObject,
    _ g: @isolated(any) () -> AnyObject, // expected-error {{parameter 'g' type '@isolated(any) () -> AnyObject' in '@abi' should match '() -> AnyObject'}}
    _ h: nonisolated(nonsending) () async -> AnyObject,
    _ i: () -> AnyObject, // expected-error {{parameter 'i' type '() -> AnyObject' in '@abi' should match '@isolated(any) () -> AnyObject'}}
    _ j: () async -> Void,
    _ k: () -> Void, // expected-error {{parameter 'k' type '() -> Void' in '@abi' should match '() async -> Void'}}
    _ l: () async -> Void, // expected-error {{parameter 'l' type '() async -> Void' in '@abi' should match '() -> Void'}}
    _ m: () -> Void,
    _ n: () throws -> Void,
    _ o: () -> Void, // expected-error {{parameter 'o' type '() -> Void' in '@abi' should match '() throws -> Void'}}
    _ p: () throws -> Void, // expected-error {{parameter 'p' type '() throws -> Void' in '@abi' should match '() -> Void'}}
    _ q: () -> Void,
    _ r: () -> Void,
    _ s: @convention(block) () -> Void, // expected-error {{parameter 's' type '@convention(block) () -> Void' in '@abi' should match '() -> Void'}}
    _ t: @convention(thin) () -> Void, // expected-error {{parameter 't' type '@convention(thin) () -> Void' in '@abi' should match '() -> Void'}}
    _ u: @convention(c) () -> Void // expected-error {{parameter 'u' type '@convention(c) () -> Void' in '@abi' should match '() -> Void'}}
  )
)
func fnExtInfoTest(
  _: () -> AnyObject,
  _: () -> AnyObject,
  _: () -> AnyObject,
  _: () -> AnyObject,
  _: () -> AnyObject,
  _: (MainActor) -> AnyObject,
  _: () -> AnyObject, // expected-note {{should match type here}}
  _: () async -> AnyObject,
  _: @isolated(any) () -> AnyObject, // expected-note {{should match type here}}
  _: () async -> Void,
  _: () async -> Void, // expected-note {{should match type here}}
  _: () -> Void, // expected-note {{should match type here}}
  _: () -> Void,
  _: () throws -> Void,
  _: () throws -> Void, // expected-note {{should match type here}}
  _: () -> Void, // expected-note {{should match type here}}
  _: () -> Void,
  _: () -> Void,
  _: () -> Void, // expected-note {{should match type here}}
  _: () -> Void, // expected-note {{should match type here}}
  _: () -> Void // expected-note {{should match type here}}
) {}

// FIXME: Not sure how to reach tryNormalizeOutermostType() generic func

@abi(
  func testMarkerProtocols<A, B: Sendable, C, D: SendableMetatype>(
    _: A, _: B, _: C, _: D,
    _: Any, _: Sendable, _: Any, _: SendableMetatype,
    _: AnyKeyPath, _: AnyKeyPath & Sendable, _: AnyKeyPath, _: AnyKeyPath & SendableMetatype,
    _: Any, _: Sendable & BitwiseCopyable, _: Any, _: SendableMetatype & BitwiseCopyable
  )
)
func testMarkerProtocols<A: Sendable, B, C: SendableMetatype, D>(
  _: A, _: B, _: C, _: D,
  _: Sendable, _: Any, _: SendableMetatype, _: Any,
  _: AnyKeyPath & Sendable, _: AnyKeyPath, _: AnyKeyPath & SendableMetatype, _: AnyKeyPath,
  _: Sendable & BitwiseCopyable, _: Any, _: SendableMetatype & BitwiseCopyable, _: Any
) {}

@abi(
  func testNormalProtocols(
    _ a: Any, // expected-error {{parameter 'a' type 'Any' in '@abi' should match 'any CustomStringConvertible'}}
    _ b: CustomStringConvertible, // expected-error {{parameter 'b' type 'any CustomStringConvertible' in '@abi' should match 'Any'}}
    _ c: AnyKeyPath, // expected-error {{parameter 'c' type 'AnyKeyPath' in '@abi' should match 'any AnyKeyPath & CustomStringConvertible'}}
    _ d: AnyKeyPath & CustomStringConvertible, // expected-error {{parameter 'd' type 'any AnyKeyPath & CustomStringConvertible' in '@abi' should match 'AnyKeyPath'}}
    _ e: Any, // expected-error {{parameter 'e' type 'Any' in '@abi' should match 'any CustomDebugStringConvertible & CustomStringConvertible'}}
    _ f: CustomStringConvertible & CustomDebugStringConvertible // expected-error {{parameter 'f' type 'any CustomDebugStringConvertible & CustomStringConvertible' in '@abi' should match 'Any'}}
  )
)
func testNormalProtocols(
  _: CustomStringConvertible, // expected-note {{should match type here}}
  _: Any, // expected-note {{should match type here}}
  _: AnyKeyPath & CustomStringConvertible, // expected-note {{should match type here}}
  _: AnyKeyPath, // expected-note {{should match type here}}
  _: CustomStringConvertible & CustomDebugStringConvertible, // expected-note {{should match type here}}
  _: Any // expected-note {{should match type here}}
) {}

@abi(
  func testNormalProtocolsGeneric<A, B: CustomStringConvertible>( // expected-error {{generic signature '<A, B where A : Copyable, A : Escapable, B : CustomStringConvertible>' in '@abi' is not compatible with '<A, B where A : CustomStringConvertible, B : Copyable, B : Escapable>'}}
    _: A, _: B
  )
)
func testNormalProtocolsGeneric<A: CustomStringConvertible, B>( // expected-note {{should match type here}}
  _: A, _: B
) {}

//
// Static/Instance and interactions with `final`
//

class ClassStaticAndFinal {
  @abi(func class00final00())
  func class00final00() {}

  @abi(class func class10final00()) // expected-error {{class method 'class10final00()' in '@abi' should be instance method to ensure ABI compatibility}} {{8-14=}}
  func class10final00() {}

  @abi(static func class20final00()) // expected-error {{static method 'class20final00()' in '@abi' should be non-final instance method to ensure ABI compatibility}} {{8-15=}}
  func class20final00() {}

  @abi(func class01final00()) // expected-error {{instance method 'class01final00()' in '@abi' should be class method to ensure ABI compatibility}} {{8-8=class }}
  class func class01final00() {}

  @abi(func class02final00()) // expected-error {{non-final instance method 'class02final00()' in '@abi' should be static method to ensure ABI compatibility}} {{8-8=static }}
  static func class02final00() {}

  @abi(class func class11final00())
  class func class11final00() {}

  @abi(class func class12final00()) // expected-error {{non-final class method 'class12final00()' in '@abi' should be static method to ensure ABI compatibility}} {{8-13=static}}
  static func class12final00() {}

  @abi(static func class21final00()) // expected-error {{static method 'class21final00()' in '@abi' should be non-final class method to ensure ABI compatibility}} {{8-14=class}}
  class func class21final00() {}

  @abi(static func class22final00())
  static func class22final00() {}

  @abi(final func class00final10()) // expected-error {{final instance method 'class00final10()' in '@abi' should be non-final instance method to ensure ABI compatibility}} {{8-14=}}
  func class00final10() {}

  @abi(class final func class10final10()) // expected-error {{final class method 'class10final10()' in '@abi' should be non-final instance method to ensure ABI compatibility}} {{8-14=}}
  func class10final10() {}

  @abi(final func class01final10()) // expected-error {{final instance method 'class01final10()' in '@abi' should be non-final class method to ensure ABI compatibility}} {{8-13=class}}
  class func class01final10() {}

  @abi(final func class02final10()) // expected-error {{final instance method 'class02final10()' in '@abi' should be static method to ensure ABI compatibility}} {{8-13=static}}
  static func class02final10() {}

  @abi(class final func class11final10()) // expected-error {{final class method 'class11final10()' in '@abi' should be non-final class method to ensure ABI compatibility}} {{14-20=}}
  class func class11final10() {}

  @abi(class final func class12final10())
  static func class12final10() {}

  @abi(func class00final01()) // expected-error {{non-final instance method 'class00final01()' in '@abi' should be final instance method to ensure ABI compatibility}} {{8-8=final }}
  final func class00final01() {}

  @abi(class func class10final01()) // expected-error {{non-final class method 'class10final01()' in '@abi' should be final instance method to ensure ABI compatibility}} {{8-13=final}}
  final func class10final01() {}

  @abi(static func class20final01()) // expected-error {{static method 'class20final01()' in '@abi' should be final instance method to ensure ABI compatibility}} {{8-14=final}}
  final func class20final01() {}

  @abi(func class01final01()) // expected-error {{non-final instance method 'class01final01()' in '@abi' should be final class method to ensure ABI compatibility}} {{8-8=final class }}
  class final func class01final01() {}

  @abi(class func class11final01()) // expected-error {{non-final class method 'class11final01()' in '@abi' should be final class method to ensure ABI compatibility}} {{8-13=final class}}
  class final func class11final01() {}

  @abi(static func class21final01())
  class final func class21final01() {}

  @abi(final func class00final11())
  final func class00final11() {}

  @abi(class final func class10final11()) //expected-error {{final class method 'class10final11()' in '@abi' should be final instance method to ensure ABI compatibility}} {{14-20=}} {{8-13=final}}
  final func class10final11() {}

  @abi(final func class01final11()) // expected-error {{final instance method 'class01final11()' in '@abi' should be final class method to ensure ABI compatibility}} {{8-13=final class}}
  class final func class01final11() {}

  @abi(class final func class11final11())
  class final func class11final11() {}
}

struct StaticAndFinal {
  @abi(func class00final00())
  func class00final00() {}

  @abi(static func class20final00()) // expected-error {{static method 'class20final00()' in '@abi' should be instance method to ensure ABI compatibility}} {{8-15=}}
  func class20final00() {}

  @abi(func class02final00()) // expected-error {{instance method 'class02final00()' in '@abi' should be static method to ensure ABI compatibility}} {{8-8=static }}
  static func class02final00() {}

  @abi(static func class22final00())
  static func class22final00() {}
}

//
// Failable Initializers
//

struct FailableInits {
  @abi(init(i11: Void))
  init(i11: Void) {}

  @abi(init?(i21: Void)) // expected-error {{cannot give non-failable initializer 'init(i21:)' the ABI of a failable initializer}} {{12-13=}}
  init(i21: Void) {}

  @abi(init!(i31: Void)) // expected-error {{cannot give non-failable initializer 'init(i31:)' the ABI of a failable initializer}} {{12-13=}}
  init(i31: Void) {}

  @abi(init(i12: Void)) // expected-error {{cannot give failable initializer 'init(i12:)' the ABI of a non-failable initializer}} {{12-12=?}}
  init?(i12: Void) {}

  @abi(init?(i22: Void))
  init?(i22: Void) {}

  @abi(init!(i32: Void))
  init?(i32: Void) {}

  @abi(init(i13: Void)) // expected-error {{cannot give failable initializer 'init(i13:)' the ABI of a non-failable initializer}} {{12-12=!}}
  init!(i13: Void) {}

  @abi(init?(i23: Void))
  init!(i23: Void) {}

  @abi(init!(i33: Void))
  init!(i33: Void) {}
}

//
// Attributes
//

// @_originallyDefinedIn -- allowed to vary
@abi(@_originallyDefinedIn(module: "Other", macOS 14) func originallyDefinedIn1())
@available(macOS 12, *) @_originallyDefinedIn(module: "Other", macOS 14) public func originallyDefinedIn1() {}

@abi(func originallyDefinedIn2())
@available(macOS 12, *) @_originallyDefinedIn(module: "Other", macOS 14) public func originallyDefinedIn2() {}

@abi(@_originallyDefinedIn(module: "Other", macOS 14) func originallyDefinedIn3())
@available(macOS 12, *) public func originallyDefinedIn3() {}

@abi(@_originallyDefinedIn(module: "Different", macOS 12) func originallyDefinedIn4())
@available(macOS 12, *) @_originallyDefinedIn(module: "Other", macOS 14) public func originallyDefinedIn4() {}

// @Sendable -- allowed to vary
@abi(@Sendable func sendable1())
@Sendable func sendable1() {}

@abi(@Sendable func sendable2())
func sendable2() {}

@abi(func sendable3())
@Sendable func sendable3() {}

// @preconcurrency -- allowed to vary
@abi(@preconcurrency func preconcurrency1())
@preconcurrency func preconcurrency1() {}

@abi(@preconcurrency func preconcurrency2())
func preconcurrency2() {}

@abi(func preconcurrency3())
@preconcurrency func preconcurrency3() {}

// @_preInverseGenerics -- allowed to vary
struct PreInverseGenerics<T: ~Copyable> {
  @abi(@_preInverseGenerics func fn1(_: consuming T))
  @_preInverseGenerics func fn1(_: consuming T) {}

  @abi(@_preInverseGenerics func fn2(_: consuming T))
  func fn2(_: consuming T) {}

  @abi(func fn3(_: consuming T))
  @_preInverseGenerics func fn3(_: consuming T) {}
}

// 'nonisolated', 'isolated' arguments, global actors -- allowed to vary
@abi(@MainActor func isolation1())
@MainActor func isolation1() {}

@abi(func isolation2())
@MainActor func isolation2() {}

@abi(@MainActor func isolation3())
func isolation3() {}

@abi(nonisolated func isolation4())
@MainActor func isolation4() {}

@abi(@MainActor func isolation5())
nonisolated func isolation5() {}

@abi(func isolation6(_: isolated some Actor))
@MainActor func isolation6(_: some Actor) {}

@abi(func isolation7(_: some Actor))
func isolation7(_: isolated some Actor) {}

@abi(@concurrent func isolation8() async)
@concurrent func isolation8() async {}

@abi(func isolation9() async)
@concurrent func isolation9() async {}

@abi(@concurrent func isolation10() async)
func isolation10() async {}

@abi(nonisolated func isolation11() async)
@concurrent func isolation11() async {}

@abi(@concurrent func isolation12() async)
nonisolated func isolation12() async {}

@abi(nonisolated(nonsending) func isolation13() async)
nonisolated(nonsending) func isolation13() async {}

@abi(func isolation14() async)
nonisolated(nonsending) func isolation14() async {}

@abi(nonisolated(nonsending) func isolation15() async)
func isolation15() async {}

@abi(nonisolated func isolation16() async)
nonisolated(nonsending) func isolation16() async {}

@abi(nonisolated(nonsending) func isolation17() async)
nonisolated func isolation17() async {}

@abi(nonisolated(nonsending) func isolation18() async)
@concurrent func isolation18() async {}

@abi(@concurrent func isolation19() async)
nonisolated(nonsending) func isolation19() async {}

// CustomAttr for property wrapper -- banned in and with '@abi'
// Banned because we would need to design behavior for its auxiliary decls.
@propertyWrapper struct PropertyWrapper {
  var wrappedValue: Int { fatalError() }
}

struct CustomAttrPropertyWrapper {
  @abi(@PropertyWrapper var v1: Int) // expected-error {{property 'v1' with a wrapper cannot also be '@abi'}}
  @PropertyWrapper var v1: Int // expected-error {{property 'v1' with a wrapper cannot also be '@abi'}}

  @abi(@PropertyWrapper var v2: Int) // expected-error {{property 'v2' with a wrapper cannot also be '@abi'}}
  var v2: Int

  @abi(var v3: Int)
  @PropertyWrapper var v3: Int // expected-error {{property 'v3' with a wrapper cannot also be '@abi'}}
}

// CustomAttr for attached macro -- see Macros/macro_expand_peers.swift
// Freestanding macro in @abi -- see Macros/macro_expand.swift

// CustomAttr for result builder -- banned in '@abi'
// Has no ABI impact on either a parameter or a decl.
@resultBuilder struct ResultBuilder {
  static func buildBlock(_ values: Int...) -> Int { values.reduce(0, +) }
}

struct CustomAttrResultBuilder {
  @abi(@ResultBuilder var v1: Int) // expected-error {{unused 'ResultBuilder' attribute in '@abi'}} {{8-22=}}
  @ResultBuilder var v1: Int { 0 }

  @abi(@ResultBuilder var v2: Int) // expected-error {{unused 'ResultBuilder' attribute in '@abi'}} {{8-22=}}
  var v2: Int { 0 }

  @abi(var v3: Int)
  @ResultBuilder var v3: Int { 0 }

  @abi(@ResultBuilder func fn11() -> Int) // expected-error {{unused 'ResultBuilder' attribute in '@abi'}} {{8-22=}}
  @ResultBuilder func fn11() -> Int { 0 }

  @abi(@ResultBuilder func fn21() -> Int) // expected-error {{unused 'ResultBuilder' attribute in '@abi'}} {{8-22=}}
  func fn21() -> Int { 0 }

  @abi(func fn31() -> Int)
  @ResultBuilder func fn31() -> Int { 0 }

  @abi(func fn12(@ResultBuilder _: () -> Int) -> Int) // expected-error {{unused 'ResultBuilder' attribute in '@abi'}}  {{18-32=}}
  func fn12(@ResultBuilder _: () -> Int) -> Int { 0 }

  @abi(func fn22(@ResultBuilder _: () -> Int) -> Int) // expected-error {{unused 'ResultBuilder' attribute in '@abi'}} {{18-32=}}
  func fn22(_: () -> Int) -> Int { 0 }

  @abi(func fn32(_: () -> Int) -> Int)
  func fn32(@ResultBuilder _: () -> Int) -> Int { 0 }
}

// NSCopying - see attr/attr_abi_objc.swift

// @LLDBDebuggerFunction -- banned in @abi
@abi(@LLDBDebuggerFunction func lldbDebuggerFunction1()) // expected-error {{unused 'LLDBDebuggerFunction' attribute in '@abi'}} {{6-27=}}
@LLDBDebuggerFunction func lldbDebuggerFunction1() {}

@abi(@LLDBDebuggerFunction func lldbDebuggerFunction2()) // expected-error {{unused 'LLDBDebuggerFunction' attribute in '@abi'}} {{6-27=}}
func lldbDebuggerFunction2() {}

@abi(func lldbDebuggerFunction3())
@LLDBDebuggerFunction func lldbDebuggerFunction3() {}

// @_compilerInitialized -- banned in @abi
class CompilerInitialized {
  @abi(@_compilerInitialized let v1: Int) // expected-error {{unused '_compilerInitialized' attribute in '@abi'}} {{8-29=}}
  @_compilerInitialized let v1: Int

  @abi(@_compilerInitialized let v2: Int) // expected-error {{unused '_compilerInitialized' attribute in '@abi'}} {{8-29=}}
  let v2: Int

  @abi(let v3: Int)
  @_compilerInitialized let v3: Int

  init() {}
}

// @_hasStorage -- banned in @abi
struct HasStorage {
  @abi(@_hasStorage let v1: Int) // expected-error {{unused '_hasStorage' attribute in '@abi'}} {{8-20=}}
  @_hasStorage let v1: Int

  @abi(@_hasStorage let v2: Int) // expected-error {{unused '_hasStorage' attribute in '@abi'}} {{8-20=}}
  let v2: Int

  @abi(let v3: Int)
  @_hasStorage let v3: Int

  init() {}
}

// @discardableResult -- banned in @abi
@abi(@discardableResult func discardableResult1() -> Int) // expected-error {{unused 'discardableResult' attribute in '@abi'}} {{6-24=}}
@discardableResult func discardableResult1() -> Int { 0 }

@abi(@discardableResult func discardableResult2() -> Int) // expected-error {{unused 'discardableResult' attribute in '@abi'}} {{6-24=}}
func discardableResult2() -> Int { 0 }

@abi(func lldbDebuggerFunction3() -> Int)
@discardableResult func discardableResult3() -> Int { 0 }

// @warn_unqualified_access -- banned in @abi
struct WarnUnqualifiedAccess {
  @abi(@warn_unqualified_access func fn1()) // expected-error {{unused 'warn_unqualified_access' attribute in '@abi'}} {{8-32=}}
  @warn_unqualified_access func fn1() {}

  @abi(@warn_unqualified_access func fn2()) // expected-error {{unused 'warn_unqualified_access' attribute in '@abi'}} {{8-32=}}
  func fn2() {}

  @abi(func fn3())
  @warn_unqualified_access func fn3() {}
}

// @_disfavoredOverload -- banned in @abi
@abi(@_disfavoredOverload func disfavoredOverload1()) // expected-error {{unused '_disfavoredOverload' attribute in '@abi'}} {{6-26=}}
@_disfavoredOverload func disfavoredOverload1() {}

@abi(@_disfavoredOverload func disfavoredOverload2()) // expected-error {{unused '_disfavoredOverload' attribute in '@abi'}} {{6-26=}}
func disfavoredOverload2() {}

@abi(func disfavoredOverload3())
@_disfavoredOverload func disfavoredOverload3() {}

// @_nonEphemeral -- banned in @abi
@abi(func nonEphemeral1(@_nonEphemeral _: UnsafeRawPointer)) // expected-error {{unused '_nonEphemeral' attribute in '@abi'}} {{25-39=}}
func nonEphemeral1(@_nonEphemeral _: UnsafeRawPointer) {}

@abi(func nonEphemeral2(@_nonEphemeral _: UnsafeRawPointer)) // expected-error {{unused '_nonEphemeral' attribute in '@abi'}} {{25-39=}}
func nonEphemeral2(_: UnsafeRawPointer) {}

@abi(func disfavoredOverload3(_: UnsafeRawPointer))
func nonEphemeral3(@_nonEphemeral _: UnsafeRawPointer) {}

// @_inheritActorContext
@abi(func inheritActorContext1(@_inheritActorContext fn: @Sendable @escaping () async -> Void))
func inheritActorContext1(@_inheritActorContext fn: @Sendable @escaping () async -> Void) {}

@abi(func inheritActorContext2(@_inheritActorContext fn: @Sendable @escaping () async -> Void))
func inheritActorContext2(fn: @Sendable @escaping () async -> Void) {}

@abi(func inheritActorContext3(fn: @Sendable @escaping () async -> Void))
func inheritActorContext3(@_inheritActorContext fn: @Sendable @escaping () async -> Void) {}

@abi(func inheritActorContext4(@_inheritActorContext(always) fn: @Sendable @escaping () async -> Void))
func inheritActorContext4(fn: @Sendable @escaping () async -> Void) {}

// @excusivity(checked/unchecked) -- banned in @abi
class Exclusivity {
  @abi(var checked00: Int)
  var checked00: Int = 0

  @abi(@exclusivity(checked) var checked10: Int) // expected-error {{unused 'exclusivity(checked)' attribute in '@abi'}} {{8-29=}}
  var checked10: Int = 0

  @abi(@exclusivity(unchecked) var checked20: Int) // expected-error {{unused 'exclusivity(unchecked)' attribute in '@abi'}} {{8-31=}}
  var checked20: Int = 0

  @abi(var checked01: Int)
  @exclusivity(checked) var checked01: Int = 0

  @abi(@exclusivity(checked) var checked11: Int) // expected-error {{unused 'exclusivity(checked)' attribute in '@abi'}} {{8-29=}}
  @exclusivity(checked) var checked11: Int = 0

  @abi(@exclusivity(unchecked) var checked21: Int) // expected-error {{unused 'exclusivity(unchecked)' attribute in '@abi'}} {{8-31=}}
  @exclusivity(checked) var checked21: Int = 0

  @abi(var checked02: Int)
  @exclusivity(unchecked) var checked02: Int = 0

  @abi(@exclusivity(checked) var checked12: Int) // expected-error {{unused 'exclusivity(checked)' attribute in '@abi'}} {{8-29=}}
  @exclusivity(unchecked) var checked12: Int = 0

  @abi(@exclusivity(unchecked) var checked22: Int) // expected-error {{unused 'exclusivity(unchecked)' attribute in '@abi'}} {{8-31=}}
  @exclusivity(unchecked) var checked22: Int = 0
}

// @_noAllocation -- banned in @abi
@abi(@_noAllocation func noAllocation1()) // expected-error {{unused '_noAllocation' attribute in '@abi'}} {{6-20=}}
@_noAllocation func noAllocation1() {}

@abi(@_noAllocation func noAllocation2()) // expected-error {{unused '_noAllocation' attribute in '@abi'}} {{6-20=}}
func noAllocation2() {}

@abi(func noAllocation3())
@_noAllocation func noAllocation3() {}

// @_noLocks -- banned in @abi
@abi(@_noLocks func noLocks1()) // expected-error {{unused '_noLocks' attribute in '@abi'}} {{6-15=}}
@_noLocks func noLocks1() {}

@abi(@_noLocks func noLocks2()) // expected-error {{unused '_noLocks' attribute in '@abi'}} {{6-15=}}
func noLocks2() {}

@abi(func noLocks3())
@_noLocks func noLocks3() {}

// @_noImplicitCopy -- banned in @abi
struct NoImplicitCopy {
  @abi(@_noImplicitCopy func fn1()) // expected-error {{unused '_noImplicitCopy' attribute in '@abi'}} {{8-24=}}
  @_noImplicitCopy func fn1() {}

  @abi(@_noImplicitCopy func fn2()) // expected-error {{unused '_noImplicitCopy' attribute in '@abi'}} {{8-24=}}
  func fn2() {}

  @abi(func fn3())
  @_noImplicitCopy func fn3() {}

  @abi(func fn4(@_noImplicitCopy _: Int)) // expected-error {{unused '_noImplicitCopy' attribute in '@abi'}} {{17-33=}}
  func fn4(@_noImplicitCopy _: Int) {}

  @abi(func fn5(@_noImplicitCopy _: Int)) // expected-error {{unused '_noImplicitCopy' attribute in '@abi'}} {{17-33=}}
  func fn5(_: Int) {}

  @abi(func fn6(_: Int))
  func fn6(@_noImplicitCopy _: Int) {}
}

// @_noObjCBridging -- banned in @abi
@abi(@_noObjCBridging func noObjCBridging1()) // expected-error {{unused '_noObjCBridging' attribute in '@abi'}} {{6-22=}}
@_noObjCBridging func noObjCBridging1() {}

@abi(@_noObjCBridging func noObjCBridging2()) // expected-error {{unused '_noObjCBridging' attribute in '@abi'}} {{6-22=}}
func noObjCBridging2() {}

@abi(func noObjCBridging3())
@_noObjCBridging func noObjCBridging3() {}

// @_noExistentials -- banned in @abi
@abi(@_noExistentials func noExistentials1()) // expected-error {{unused '_noExistentials' attribute in '@abi'}} {{6-22=}}
@_noExistentials func noExistentials1() {}

@abi(@_noExistentials func noExistentials2()) // expected-error {{unused '_noExistentials' attribute in '@abi'}} {{6-22=}}
func noExistentials2() {}

@abi(func noExistentials3())
@_noExistentials func noExistentials3() {}

// @_noRuntime -- banned in @abi
@abi(@_noRuntime func noRuntime1()) // expected-error {{unused '_noRuntime' attribute in '@abi'}} {{6-17=}}
@_noRuntime func noRuntime1() {}

@abi(@_noRuntime func noRuntime2()) // expected-error {{unused '_noRuntime' attribute in '@abi'}} {{6-17=}}
func noRuntime2() {}

@abi(func noRuntime3())
@_noRuntime func noRuntime3() {}

// @_noEagerMove -- banned in @abi
struct NoEagerMove {
  @abi(@_noEagerMove func fn1()) // expected-error {{unused '_noEagerMove' attribute in '@abi'}} {{8-21=}}
  @_noEagerMove func fn1() {}

  @abi(@_noEagerMove func fn2()) // expected-error {{unused '_noEagerMove' attribute in '@abi'}} {{8-21=}}
  func fn2() {}

  @abi(func fn3())
  @_noEagerMove func fn3() {}

  @abi(func fn4(@_noEagerMove _: Int)) // expected-error {{unused '_noEagerMove' attribute in '@abi'}} {{17-30=}}
  func fn4(@_noEagerMove _: Int) {}

  @abi(func fn5(@_noEagerMove _: Int)) // expected-error {{unused '_noEagerMove' attribute in '@abi'}} {{17-30=}}
  func fn5(_: Int) {}

  @abi(func fn6(_: Int))
  func fn6(@_noEagerMove _: Int) {}
}

// @_eagerMove -- banned in @abi
struct EagerMove {
  @abi(@_eagerMove func fn1()) // expected-error {{unused '_eagerMove' attribute in '@abi'}} {{8-19=}}
  @_eagerMove func fn1() {}

  @abi(@_eagerMove func fn2()) // expected-error {{unused '_eagerMove' attribute in '@abi'}} {{8-19=}}
  func fn2() {}

  @abi(func fn3())
  @_eagerMove func fn3() {}

  @abi(func fn4(@_eagerMove _: Int)) // expected-error {{unused '_eagerMove' attribute in '@abi'}} {{17-28=}}
  func fn4(@_eagerMove _: Int) {}

  @abi(func fn5(@_eagerMove _: Int)) // expected-error {{unused '_eagerMove' attribute in '@abi'}} {{17-28=}}
  func fn5(_: Int) {}

  @abi(func fn6(_: Int))
  func fn6(@_eagerMove _: Int) {}
}

// @_lexicalLifetimes -- banned in @abi
@abi(@_lexicalLifetimes func lexicalLifetimes1()) // expected-error {{unused '_lexicalLifetimes' attribute in '@abi'}} {{6-24=}}
@_lexicalLifetimes func lexicalLifetimes1() {}

@abi(@_lexicalLifetimes func lexicalLifetimes2()) // expected-error {{unused '_lexicalLifetimes' attribute in '@abi'}} {{6-24=}}
func lexicalLifetimes2() {}

@abi(func lexicalLifetimes3())
@_lexicalLifetimes func lexicalLifetimes3() {}

// @_assemblyVision -- banned in @abi
@abi(@_assemblyVision func assemblyVision1()) // expected-error {{unused '_assemblyVision' attribute in '@abi'}} {{6-22=}}
@_assemblyVision func assemblyVision1() {}

@abi(@_assemblyVision func assemblyVision2()) // expected-error {{unused '_assemblyVision' attribute in '@abi'}} {{6-22=}}
func assemblyVision2() {}

@abi(func assemblyVision3())
@_assemblyVision func assemblyVision3() {}

// @_extern -- banned in @abi
@abi(@_extern(c) @_extern(wasm, module: "foo", name: "bar") func extern1()) // expected-error {{unused '_extern' attribute in '@abi'}} {{18-61=}}  expected-error {{unused '_extern' attribute in '@abi'}} {{6-17=}}
@_extern(c) @_extern(wasm, module: "foo", name: "bar") func extern1()

@abi(@_extern(c) @_extern(wasm, module: "foo", name: "bar") func extern2()) // expected-error {{unused '_extern' attribute in '@abi'}} {{18-61=}} expected-error {{unused '_extern' attribute in '@abi'}} {{6-17=}}
func extern2() {}

@abi(func extern3())
@_extern(c) @_extern(wasm, module: "foo", name: "bar") func extern3()

// @_used -- banned in @abi
@abi(@_used func used1()) // expected-error {{unused '_used' attribute in '@abi'}} {{6-12=}}
@_used func used1() {}

@abi(@_used func used2()) // expected-error {{unused '_used' attribute in '@abi'}} {{6-12=}}
func used2() {}

@abi(func used3())
@_used func used3() {}

// weak, unowned, unowned(unsafe) -- banned in @abi
class ReferenceOwnership {
  @abi(var v00: AnyObject?)
  var v00: AnyObject? = nil

  @abi(weak var v10: AnyObject?) // expected-error {{unused 'weak' modifier in '@abi'}} {{8-12=}}
  var v10: AnyObject? = nil

  @abi(unowned var v20: AnyObject?) // expected-error {{unused 'unowned' modifier in '@abi'}} {{8-15=}}
  var v20: AnyObject? = nil

  @abi(unowned(unsafe) var v30: AnyObject?) // expected-error {{unused 'unowned(unsafe)' modifier in '@abi'}} {{8-23=}}
  var v30: AnyObject? = nil

  @abi(var v01: AnyObject?)
  weak var v01: AnyObject? = nil

  @abi(weak var v11: AnyObject?) // expected-error {{unused 'weak' modifier in '@abi'}} {{8-12=}}
  weak var v11: AnyObject? = nil

  @abi(unowned var v21: AnyObject?) // expected-error {{unused 'unowned' modifier in '@abi'}} {{8-15=}}
  weak var v21: AnyObject? = nil

  @abi(unowned(unsafe) var v31: AnyObject?) // expected-error {{unused 'unowned(unsafe)' modifier in '@abi'}} {{8-23=}}
  weak var v31: AnyObject? = nil

  @abi(var v02: AnyObject?)
  unowned var v02: AnyObject? = nil

  @abi(weak var v12: AnyObject?) // expected-error {{unused 'weak' modifier in '@abi'}} {{8-12=}}
  unowned var v12: AnyObject? = nil

  @abi(unowned var v22: AnyObject?) // expected-error {{unused 'unowned' modifier in '@abi'}} {{8-15=}}
  unowned var v22: AnyObject? = nil

  @abi(unowned(unsafe) var v32: AnyObject?) // expected-error {{unused 'unowned(unsafe)' modifier in '@abi'}} {{8-23=}}
  unowned var v32: AnyObject? = nil

  @abi(var v03: AnyObject?)
  unowned(unsafe) var v03: AnyObject? = nil

  @abi(weak var v13: AnyObject?) // expected-error {{unused 'weak' modifier in '@abi'}} {{8-12=}}
  unowned(unsafe) var v13: AnyObject? = nil

  @abi(unowned var v23: AnyObject?) // expected-error {{unused 'unowned' modifier in '@abi'}} {{8-15=}}
  unowned(unsafe) var v23: AnyObject? = nil

  @abi(unowned(unsafe) var v33: AnyObject?) // expected-error {{unused 'unowned(unsafe)' modifier in '@abi'}} {{8-23=}}
  unowned(unsafe) var v33: AnyObject? = nil
}

// @abi -- banned in @abi (no recursion)
@abi(
  @abi(func abiRecursion()) // expected-error {{unused 'abi' attribute in '@abi'}} {{3-29=}}
  func abiRecursion()
)
func abiRecursion() {}

// @unsafe -- banned in @abi
@abi(@unsafe func unsafe1()) // expected-error {{unused 'unsafe' attribute in '@abi'}} {{6-13=}}
@unsafe func unsafe1() {}

@abi(@unsafe func unsafe2()) // expected-error {{unused 'unsafe' attribute in '@abi'}} {{6-13=}}
func unsafe2() {}

@abi(func unsafe3())
@unsafe func unsafe3() {}

// @safe -- banned in @abi
@abi(@safe func safe1()) // expected-error {{unused 'safe' attribute in '@abi'}} {{6-11=}}
@safe func safe1() {}

@abi(@safe func safe2()) // expected-error {{unused 'safe' attribute in '@abi'}} {{6-11=}}
func safe2() {}

@abi(func safe3())
@safe func safe3() {}

// Access control, @usableFromInline, @_spi -- banned in @abi
// An ABI-only decl gets its access control from its counterpart.
@abi(internal func accessControl1()) // expected-error {{unused 'internal' modifier in '@abi'}} {{6-14=}}
func accessControl1() {}

@abi(func accessControl2())
public func accessControl2() {}

@abi(@usableFromInline func accessControl3()) // expected-error {{'@usableFromInline' attribute can only be applied to internal or package declarations, but global function 'accessControl3()' is public}}
public func accessControl3() {}

@abi(private(set) var setterAccess1: Int) // expected-error {{unused 'private' modifier in '@abi'}} {{6-18=}}
var setterAccess1: Int = 42

@abi(var setterAccess2: Int)
private(set) var setterAccess2: Int = 42

@abi(@usableFromInline func usableFromInline1()) // expected-error {{unused 'usableFromInline' attribute in '@abi'}} {{6-23=}}
@usableFromInline func usableFromInline1() {}

@abi(func usableFromInline2())
@usableFromInline func usableFromInline2() {}

@_spi(foo) public struct SPIType {} // expected-note 2 {{struct declared here}}

@abi(@_spi(foo) func spi1(_: SPIType)) // expected-error {{unused '_spi' attribute in '@abi'}} {{6-16=}}
@_spi(foo) public func spi1(_: SPIType) {}

@abi(func spi2(_: SPIType))
@_spi(foo) public func spi2(_: SPIType) {}

@abi(func spi3(_: SPIType)) // expected-error {{cannot use struct 'SPIType' here; it is SPI}}
public func spi3(_: SPIType) {} // expected-error {{cannot use struct 'SPIType' here; it is SPI}}

// @available, @_unavailable*, @backDeployed -- banned in @abi 
// An ABI-only decl gets its availability from its counterpart.
@abi(@available(macOS 14, iOS 16, *) func available1()) // expected-error {{unused 'available' attribute in '@abi'}} {{6-37=}}
@available(macOS 14, iOS 16, *) func available1() {}

@abi(@available(macOS 14, iOS 16, *) func available2()) // expected-error {{unused 'available' attribute in '@abi'}} {{6-37=}}
func available2() {}

@abi(func available3())
@available(macOS 14, iOS 16, *) func available3() {}

@abi(
  @available(macOS, unavailable) // expected-error {{unused 'available' attribute in '@abi'}} {{3-34=}}
  @available(iOS, deprecated) // expected-error {{unused 'available' attribute in '@abi'}} {{3-31=}}
  func available4()
)
@available(macOS 14, iOS 16, *) func available4() {}

// Additional tests in attr/attr_abi_objc.swift

@abi(@_unavailableFromAsync func unavailableFromAsync1()) // expected-error {{unused '_unavailableFromAsync' attribute in '@abi'}} {{6-28=}}
@_unavailableFromAsync func unavailableFromAsync1() {}

@abi(@_unavailableFromAsync func unavailableFromAsync2()) // expected-error {{unused '_unavailableFromAsync' attribute in '@abi'}} {{6-28=}}
func unavailableFromAsync2() {}

@abi(func unavailableFromAsync3())
@_unavailableFromAsync func unavailableFromAsync3() {}

// FIXME: Test @_unavailableInEmbedded (it gets rewritten in the parser)

@abi(@backDeployed(before: macOS 14) func backDeployed1()) // expected-error {{unused 'backDeployed' attribute in '@abi'}} {{6-37=}}
@backDeployed(before: macOS 14) public func backDeployed1() {}

@abi(@backDeployed(before: macOS 14) func backDeployed2()) // expected-error {{unused 'backDeployed' attribute in '@abi'}} {{6-37=}}
public func backDeployed2() {}

@abi(func backDeployed3())
@backDeployed(before: macOS 14) public func backDeployed3() {}

// override, @_nonoverride -- banned in @abi
// An ABI-only decl gets its overrides from its counterpart; no marker modifiers
// are required.
class Overridden {
  func fn1() {}

  func fn2() {} // expected-note 2 {{overridden declaration is here}}

  func fn3() {}
}

class Override: Overridden {
  @abi(override func fn1()) // expected-error {{unused 'override' modifier in '@abi'}} {{8-16=}}
  override func fn1() {}

  @abi(override func fn2()) // expected-error {{unused 'override' modifier in '@abi'}} {{8-16=}}
  func fn2() {} // expected-error {{overriding declaration requires an 'override' keyword}}

  @abi(func fn3())
  override func fn3() {}
}

class NonOverride: Overridden {
  @abi(@_nonoverride func fn1()) // expected-error {{unused '_nonoverride' attribute in '@abi'}} {{8-21=}}
  @_nonoverride func fn1() {}

  @abi(@_nonoverride func fn2()) // expected-error {{unused '_nonoverride' attribute in '@abi'}} {{8-21=}}
  func fn2() {} // expected-error {{overriding declaration requires an 'override' keyword}}

  @abi(func fn3())
  @_nonoverride func fn3() {}
}

// @_silgen_name -- banned in @abi *and* on declarations with @abi
// Becuase of the way @_silgen_name is implemented, these would interact oddly
// if they were allowed on the same decl.
@_silgen_name("conflictingAttrsSilgenName")
@abi(func silgenName1())
func silgenName1() {} // expected-error@-2 {{cannot use '@_silgen_name' and '@abi' on the same global function because they serve the same purpose}} {{1-44=}}

@abi(@_silgen_name("silgenNameWithABI") func silgenName2()) // expected-error {{unused '_silgen_name' attribute in '@abi'}} {{6-40=}}
func silgenName2() {}

// @_documentation(visibility:metadata:) -- banned in @abi
@abi(@_documentation(visibility: public) func documentation1()) // expected-error {{unused '_documentation' attribute in '@abi'}} {{6-41=}}
@_documentation(visibility: public) func documentation1() {}

@abi(@_documentation(visibility: public) func documentation2()) // expected-error {{unused '_documentation' attribute in '@abi'}} {{6-41=}}
func documentation2() {}

@abi(func documentation3())
@_documentation(visibility: public) func documentation3() {}

// @_allowFeatureSuppression -- banned in @abi
// Feature suppression should be applied to the API since we can't put `#if`
// inside `@abi`.
@abi(@_allowFeatureSuppression(IsolatedAny) func allowFeatureSuppression1()) // expected-error {{unused '_allowFeatureSuppression' attribute in '@abi'}} {{6-44=}}
@_allowFeatureSuppression(IsolatedAny) func allowFeatureSuppression1() {}

@abi(@_allowFeatureSuppression(IsolatedAny) func allowFeatureSuppression2()) // expected-error {{unused '_allowFeatureSuppression' attribute in '@abi'}} {{6-44=}}
func allowFeatureSuppression2() {}

@abi(func allowFeatureSuppression3())
@_allowFeatureSuppression(IsolatedAny) func allowFeatureSuppression3() {}

// @objc -- tested in attr/attr_abi_objc.swift
// @IBAction -- tested in attr/attr_abi_objc.swift
// @IBInspectable -- tested in attr/attr_abi_objc.swift
// @GKInspectable -- tested in attr/attr_abi_objc.swift
// @IBOutlet -- tested in attr/attr_abi_objc.swift
// @IBSegueAction -- tested in attr/attr_abi_objc.swift
// @NSManaged -- tested in attr/attr_abi_objc.swift
// @nonobjc -- tested in attr/attr_abi_objc.swift
// optional -- tested in attr/attr_abi_objc.swift
// dynamic -- tested in attr/attr_abi_objc.swift

// @_cdecl -- banned in @abi
// ABI-only decls inherit cdecl-ness from their counterpart
@abi(@_cdecl("cdecl1") func cdecl1()) // expected-error {{unused '_cdecl' attribute in '@abi'}} {{6-23=}}
@_cdecl("cdecl1") func cdecl1() {}

@abi(@_cdecl("cdecl2") func cdecl2()) // expected-error {{unused '_cdecl' attribute in '@abi'}} {{6-23=}}
func cdecl2() {}

@abi(func cdecl3())
@_cdecl("cdecl3") func cdecl3() {}

// @implementation -- banned in @abi
// ABI-only decls inherit implementation-ness from their counterpart
@abi(@implementation func implementation1()) // expected-error {{unused 'implementation' attribute in '@abi'}} {{6-21=}}
@_cdecl("implementation1") @implementation func implementation1() {}

@abi(@implementation func implementation2()) // expected-error {{unused 'implementation' attribute in '@abi'}} {{6-21=}}
@_cdecl("implementation2") func implementation2() {}

@abi(func implementation3())
@_cdecl("implementation3") @implementation func implementation3() {}

// @_expose -- banned in @abi
// ABI-only decls inherit exposure from their counterpart
@abi(@_expose(Cxx) func expose1()) // expected-error {{unused '_expose' attribute in '@abi'}} {{6-19=}}
@_expose(Cxx) func expose1() {}

@abi(@_expose(Cxx) func expose2()) // expected-error {{unused '_expose' attribute in '@abi'}} {{6-19=}}
func expose2() {}

@abi(func expose3())
@_expose(Cxx) func expose3() {}

// @_section -- banned in @abi
@abi(@_section("fnord") func section1()) // expected-error {{unused '_section' attribute in '@abi'}} {{6-24=}}
@_section("fnord") func section1() {}

@abi(@_section("fnord") func section2()) // expected-error {{unused '_section' attribute in '@abi'}} {{6-24=}}
func section2() {}

@abi(func section3())
@_section("fnord") func section3() {}

// @inlinable -- banned in @abi
// Although the inlining *does* occasionally get mangled, it's only done in the
// SpecializationManglers, which shouldn't get their serialization from an ABI
// attribute.
@abi(@inlinable func inlinable1()) // expected-error {{unused 'inlinable' attribute in '@abi'}} {{6-16=}}
@inlinable func inlinable1() {}

@abi(@inlinable func inlinable2()) // expected-error {{unused 'inlinable' attribute in '@abi'}} {{6-16=}}
func inlinable2() {}

@abi(func inlinable3())
@inlinable func inlinable3() {}

// @inlinable -- banned in @abi
@abi(@inline(never) func inline1()) // expected-error {{unused 'inline(never)' attribute in '@abi'}} {{6-20=}}
@inline(never) func inline1() {}

@abi(@inline(never) func inline2()) // expected-error {{unused 'inline(never)' attribute in '@abi'}} {{6-20=}}
func inline2() {}

@abi(func inline3())
@inline(never) func inline3() {}

// @_transparent -- banned in @abi
@abi(@_transparent func transparent1()) // expected-error {{unused '_transparent' attribute in '@abi'}} {{6-19=}}
@_transparent func transparent1() {}

@abi(@_transparent func transparent2()) // expected-error {{unused '_transparent' attribute in '@abi'}} {{6-19=}}
func transparent2() {}

@abi(func transparent3())
@_transparent func transparent3() {}

// @_alwaysEmitIntoClient -- banned in @abi
@abi(@_alwaysEmitIntoClient func alwaysEmitIntoClient1()) // expected-error {{unused '_alwaysEmitIntoClient' attribute in '@abi'}} {{6-28=}}
@_alwaysEmitIntoClient func alwaysEmitIntoClient1() {}

@abi(@_alwaysEmitIntoClient func alwaysEmitIntoClient2()) // expected-error {{unused '_alwaysEmitIntoClient' attribute in '@abi'}} {{6-28=}}
func alwaysEmitIntoClient2() {}

@abi(func alwaysEmitIntoClient3())
@_alwaysEmitIntoClient func alwaysEmitIntoClient3() {}

// @_optimize(none) -- banned in @abi
@abi(@_optimize(none) func optimize1()) // expected-error {{unused '_optimize(none)' attribute in '@abi'}} {{6-22=}}
@_optimize(none) func optimize1() {}

@abi(@_optimize(none) func optimize2()) // expected-error {{unused '_optimize(none)' attribute in '@abi'}} {{6-22=}}
func optimize2() {}

@abi(func optimize3())
@_optimize(none) func optimize3() {}

// convenience -- must match in @abi
// This doesn't have direct mangling impact, but a future direction where
// convenience inits could fake designated inits or vice versa might be useful.
class Convenience {
  @abi(convenience init(i1: Void))
  convenience init(i1: Void) { fatalError() }

  @abi(convenience init(i2: Void))  // expected-error {{extra 'convenience' modifier in '@abi'}} {{8-19=}}
  init(i2: Void) { fatalError() }

  @abi(init(i3: Void)) // expected-error {{missing 'convenience' modifier in '@abi'}} {{8-8=convenience }}
  convenience init(i3: Void) { fatalError() } // expected-note {{should match modifier here}}
}

// required -- must match in @abi
// This doesn't have direct mangling impact, but a future direction where
// required inits could fake normal inits or vice versa might be useful.
class Required {
  @abi(required init(i1: Void))
  required init(i1: Void) { fatalError() }

  @abi(required init(i2: Void))  // expected-error {{extra 'required' modifier in '@abi'}} {{8-16=}}
  init(i2: Void) { fatalError() }

  @abi(init(i3: Void)) // expected-error {{missing 'required' modifier in '@abi'}} {{8-8=required }}
  required init(i3: Void) { fatalError() } // expected-note {{should match modifier here}}
}

// lazy -- banned both in and with @abi
// This introduces auxiliary decls whose ABI could not be controlled.
class Lazy {
  @abi(lazy var v1: Int) // expected-error {{'lazy' is not compatible with '@abi' attribute}} {{8-12=}}
  lazy var v1: Int = 0 // expected-error {{'lazy' is not compatible with '@abi' attribute}} {{3-8=}}

  @abi(lazy var v2: Int) // expected-error {{'lazy' is not compatible with '@abi' attribute}} {{8-12=}}
  var v2: Int = 0

  @abi(var v3: Int)
  lazy var v3: Int = 0 // expected-error {{'lazy' is not compatible with '@abi' attribute}} {{3-8=}}
}

// @_fixed_layout -- banned in @abi
class FixedLayoutVars {
  @abi(@_fixed_layout var v1: Int) // expected-error {{unused '_fixed_layout' attribute in '@abi'}} {{8-22=}}
  @_fixed_layout public var v1: Int = 0

  @abi(@_fixed_layout var v2: Int) // expected-error {{unused '_fixed_layout' attribute in '@abi'}} {{8-22=}}
  public var v2: Int = 0

  @abi(var v3: Int)
  @_fixed_layout public var v3: Int = 0
}

// @_specialize -- banned in @abi
// TODO: Maybe use @_specialize in @abi to tweak the ABI of specializations.
// Ban it for now, since there's nothing useful you can do with it yet.
@abi(@_specialize(where T == Int) func specialize1<T>(_: T)) // expected-error {{unused '_specialize' attribute in '@abi'}} {{6-34=}}
@_specialize(where T == Int) func specialize1<T>(_: T) {}

@abi(@_specialize(where T == Int) func specialize2<T>(_: T)) // expected-error {{unused '_specialize' attribute in '@abi'}} {{6-34=}}
func specialize2<T>(_: T) {}

@abi(func specialize3<T>(_: T))
@_specialize(where T == Int) func specialize3<T>(_: T) {}

// @_effects -- banned in @abi
@abi(@_effects(readonly) func effects1()) // expected-error {{unused '_effects(readonly)' attribute in '@abi'}} {{6-25=}}
@_effects(readonly) func effects1() {}

@abi(@_effects(readonly) func effects2()) // expected-error {{unused '_effects(readonly)' attribute in '@abi'}} {{6-25=}}
func effects2() {}

@abi(func effects3())
@_effects(readonly) func effects3() {}

// @_implements -- banned in @abi
protocol ImplementsProto {
  func f1()
  func f2()
  func f3()
}

class Implements: ImplementsProto {
  @abi(@_implements(ImplementsProto, f1) func f1()) // expected-error {{unused '_implements' attribute in '@abi'}} {{8-41=}}
  @_implements(ImplementsProto, f1) func f1() {}

  @abi(@_implements(ImplementsProto, f2) func f2()) // expected-error {{unused '_implements' attribute in '@abi'}} {{8-41=}}
  func f2() {}

  @abi(func f3())
  @_implements(ImplementsProto, f3) func f3() {}
}

// @_dynamicReplacement -- banned in @abi
struct DynamicReplacement {
  dynamic func f1Original() {}
  dynamic func f2Original() {}
  dynamic func f3Original() {}
}

extension DynamicReplacement {
  @abi(@_dynamicReplacement(for: f1Original) func f1()) // expected-error {{unused '_dynamicReplacement' attribute in '@abi'}} {{8-45=}}
  @_dynamicReplacement(for: f1Original) func f1() {}

  @abi(@_dynamicReplacement(for: f2Original) func f2()) // expected-error {{unused '_dynamicReplacement' attribute in '@abi'}} {{8-45=}}
  func f2() {}

  @abi(func f3())
  @_dynamicReplacement(for: f3Original) func f3() {}
}

// @_weakLinked -- tested in attr/attr_weaklinked.swift

// @_borrowed -- banned in @abi
protocol BorrowedAttr {
  @abi(@_borrowed var v1: Int) // expected-error {{unused '_borrowed' attribute in '@abi'}} {{8-18=}}
  @_borrowed var v1: Int { get set }

  @abi(var v2: Int)
  @_borrowed var v2: Int { get set }

  @abi(@_borrowed var v3: Int) // expected-error {{unused '_borrowed' attribute in '@abi'}} {{8-18=}}
  var v3: Int { get set }
}

// @lifetime -- must match in @abi
// TODO: Probably possible to make these unconstrained as long as we ensure
// that `@_addressableForDependencies` doesn't cause a calling convention
// change.
struct Lifetime: ~Escapable {
  @abi(@lifetime(borrow i1) init(i1: UnsafeRawPointer))
  @lifetime(borrow i1) init(i1: UnsafeRawPointer) {}

  @abi(@lifetime(borrow i2) init(i2: UnsafeRawPointer)) // expected-error {{extra 'lifetime' attribute in '@abi'}} {{8-28=}}
  init(i2: UnsafeRawPointer) {}

  @abi(init(i3: UnsafeRawPointer)) // expected-error {{missing 'lifetime' attribute in '@abi'}} {{8-8=@lifetime(borrow i3) }}
  @lifetime(borrow i3) init(i3: UnsafeRawPointer) {} // expected-note {{should match attribute here}}

  @abi(@lifetime(borrow i4) init(i4: UnsafeRawPointer, i4a: UnsafeRawPointer)) // expected-error {{'lifetime' attribute in '@abi' should match '@lifetime(borrow i4a)'}} {{8-28=@lifetime(borrow i4a)}}
  @lifetime(borrow i4a) init(i4: UnsafeRawPointer, i4a: UnsafeRawPointer) {} // expected-note {{should match attribute here}}
}

// @_unsafeNonescapableResult -- must match in @abi
// TODO: This could probably be allowed to vary in some circumstances.
struct UnsafeNonescapableResult: ~Escapable {
  @abi(@_unsafeNonescapableResult init(i1: UnsafeRawPointer))
  @_unsafeNonescapableResult init(i1: UnsafeRawPointer) {}

  @abi(@_unsafeNonescapableResult init(i2: UnsafeRawPointer)) // expected-error {{extra '_unsafeNonescapableResult' attribute in '@abi'}} {{8-34=}}
  init(i2: UnsafeRawPointer) {}

  @abi(init(i3: UnsafeRawPointer)) // expected-error {{missing '_unsafeNonescapableResult' attribute in '@abi'}} {{8-8=@_unsafeNonescapableResult }}
  @_unsafeNonescapableResult init(i3: UnsafeRawPointer) {} // expected-note {{should match attribute here}}
}

// distributed -- must match in @abi
@available(SwiftStdlib 5.7, *)
distributed actor Local {
  @abi(distributed func fn1())
  distributed func fn1() {}

  @abi(distributed func fn2()) // expected-error {{extra 'distributed' modifier in '@abi'}} {{8-19=}}
  func fn2() {}

  @abi(func fn3()) // expected-error {{missing 'distributed' modifier in '@abi'}} {{8-8=distributed }}
  distributed func fn3() {} // expected-note {{should match modifier here}}
}

// _const -- allowed to vary
@abi(func const1(_: _const Int))
func const1(_: _const Int) {}

@abi(func const2(_: _const Int))
func const2(_: Int) {}

@abi(func const3(_: Int))
func const3(_: _const Int) {}

// @derivative, @differentiable, @transpose, @_noDerivative -- banned in @abi
// Too complex to infer or check
// TODO: Figure out if there's something we could do here.
@abi(@differentiable(reverse) func differentiable1(_ x: Float) -> Float) // expected-error {{unused 'differentiable' attribute in '@abi'}} {{6-30=}}
@differentiable(reverse) func differentiable1(_ x: Float) -> Float { x }

@abi(@differentiable(reverse) func differentiable2(_ x: Float) -> Float) // expected-error {{unused 'differentiable' attribute in '@abi'}} {{6-30=}}
func differentiable2(_ x: Float) -> Float { x }

@abi(func differentiable3(_ x: Float) -> Float)
@differentiable(reverse) func differentiable3(_ x: Float) -> Float { x }

@abi(
  @derivative(of: differentiable1(_:)) // expected-error {{unused 'derivative' attribute in '@abi'}} {{3-40=}}
  func derivative1(_: Float) -> (value: Float, differential: (Float) -> (Float))
)
@derivative(of: differentiable1(_:))
func derivative1(_ x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

@abi(
  @derivative(of: differentiable2(_:)) // expected-error {{unused 'derivative' attribute in '@abi'}} {{3-40=}}
  func derivative2(_: Float) -> (value: Float, differential: (Float) -> (Float))
)
func derivative2(_ x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

@abi(
  func derivative3(_: Float) -> (value: Float, differential: (Float) -> (Float))
)
@derivative(of: differentiable3(_:))
func derivative3(_ x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

struct Transpose<T: Differentiable & AdditiveArithmetic> where T == T.TangentVector {
  func fn1(_ x: T, _ y: T) -> T { x + y }
  func fn2(_ x: T, _ y: T) -> T { x + y }
  func fn3(_ x: T, _ y: T) -> T { x + y }

  @abi(
    @transpose(of: fn1, wrt: (0, 1)) // expected-error {{unused 'transpose' attribute in '@abi'}} {{5-38=}}
    func t_fn1(_ result: T) -> (T, T)
  )
  @transpose(of: fn1, wrt: (0, 1))
  func t_fn1(_ result: T) -> (T, T) { (result, result) }

  @abi(
    @transpose(of: fn2, wrt: (0, 1)) // expected-error {{unused 'transpose' attribute in '@abi'}} {{5-38=}}
    func t_fn2(_ result: T) -> (T, T)
  )
  func t_fn2(_ result: T) -> (T, T) { (result, result) }

  @abi(
    func t_fn3(_ result: T) -> (T, T)
  )
  @transpose(of: fn3, wrt: (0, 1))
  func t_fn3(_ result: T) -> (T, T) { (result, result) }
}

struct NoDerivative {
  @abi(@noDerivative func fn1()) // expected-error {{unused 'noDerivative' attribute in '@abi'}} {{8-21=}}
  @noDerivative func fn1() {}

  @abi(@noDerivative func fn2()) // expected-error {{unused 'noDerivative' attribute in '@abi'}} {{8-21=}}
  func fn2() {}

  @abi(func fn3())
  @noDerivative func fn3() {}
}

// prefix, postfix -- allowed to vary
// Essentially part of the name, which is unconstrained.
prefix operator ←
prefix operator ↑ // expected-note {{prefix operator found here}}
prefix operator → // expected-note {{prefix operator found here}}
prefix operator ↓

struct Prefix {
  @abi(static prefix func ← (value: Self) -> Self)
  static prefix func ← (value: Self) -> Self { value }

  @abi(static prefix func ↑ (value: Self) -> Self)
  static func ↑ (value: Self) -> Self { value } // expected-error {{prefix unary operator missing 'prefix' modifier}}

  @abi(static func → (value: Self) -> Self) // expected-error {{prefix unary operator missing 'prefix' modifier}}
  static prefix func → (value: Self) -> Self { value }

  // Test ABI-preserving replacement code pattern:

  @abi(static prefix func ↓ (value: Self) -> Self)
  static func __oldDownArrow(value: Self) -> Self { value }

  @abi(static func __newDownArrow(value: Self) -> Self)
  static prefix func ↓ (value: Self) -> Self { value }
}

postfix operator ←←
postfix operator ↑↑ // expected-note {{postfix operator found here}}
postfix operator →→ // expected-note {{postfix operator found here}}
postfix operator ↓↓

struct Postfix {
  @abi(static postfix func ←← (value: Self) -> Self)
  static postfix func ←← (value: Self) -> Self { value }

  @abi(static postfix func ↑↑ (value: Self) -> Self)
  static func ↑↑ (value: Self) -> Self { value } // expected-error {{postfix unary operator missing 'postfix' modifier}}

  @abi(static func →→ (value: Self) -> Self) // expected-error {{postfix unary operator missing 'postfix' modifier}}
  static postfix func →→ (value: Self) -> Self { value }

  // Test ABI-preserving replacement code pattern:

  @abi(static postfix func ↓↓ (value: Self) -> Self)
  static func __oldDownArrow(value: Self) -> Self { value }

  @abi(static func __newDownArrow(value: Self) -> Self)
  static postfix func ↓↓ (value: Self) -> Self { value }
}

// Not testing `infix`; it's not *really* valid on funcs.

// nonmutating, borrowing, consuming, __consuming, mutating -- allowed to vary
// Act like param modifiers; checked against each other separately
struct SelfParamOwnership {
  @abi(func fn00())
  func fn00() {}

  @abi(nonmutating func fn10())
  func fn10() {}

  @abi(borrowing func fn20())
  func fn20() {}

  @abi(consuming func fn30()) // expected-error {{modifier 'consuming' on self parameter in '@abi' is not compatible with default}} {{none}}
  func fn30() {} // expected-note {{should match modifier here}}

  @abi(__consuming func fn40()) // expected-error {{modifier '__consuming' on self parameter in '@abi' is not compatible with default}} {{none}}
  func fn40() {} // expected-note {{should match modifier here}}

  @abi(mutating func fn50()) // expected-error {{modifier 'mutating' on self parameter in '@abi' is not compatible with default}} {{none}}
  func fn50() {} // expected-note {{should match modifier here}}

  @abi(func fn01())
  nonmutating func fn01() {}

  @abi(nonmutating func fn11())
  nonmutating func fn11() {}

  @abi(borrowing func fn21())
  nonmutating func fn21() {}

  @abi(consuming func fn31()) // expected-error {{modifier 'consuming' on self parameter in '@abi' is not compatible with default}} {{none}}
  nonmutating func fn31() {} // expected-note {{should match modifier here}}

  @abi(__consuming func fn41()) // expected-error {{modifier '__consuming' on self parameter in '@abi' is not compatible with default}} {{none}}
  nonmutating func fn41() {} // expected-note {{should match modifier here}}

  @abi(mutating func fn51()) // expected-error {{modifier 'mutating' on self parameter in '@abi' is not compatible with default}} {{none}}
  nonmutating func fn51() {} // expected-note {{should match modifier here}}

  @abi(func fn02())
  borrowing func fn02() {}

  @abi(nonmutating func fn12())
  borrowing func fn12() {}

  @abi(borrowing func fn22())
  borrowing func fn22() {}

  @abi(consuming func fn32()) // expected-error {{modifier 'consuming' on self parameter in '@abi' is not compatible with 'borrowing'}} {{none}}
  borrowing func fn32() {} // expected-note {{should match modifier here}}

  @abi(__consuming func fn42()) // expected-error {{modifier '__consuming' on self parameter in '@abi' is not compatible with 'borrowing'}} {{none}}
  borrowing func fn42() {} // expected-note {{should match modifier here}}

  @abi(mutating func fn52()) // expected-error {{modifier 'mutating' on self parameter in '@abi' is not compatible with 'borrowing'}} {{none}}
  borrowing func fn52() {} // expected-note {{should match modifier here}}

  @abi(func fn03()) // expected-error {{default modifier on self parameter in '@abi' is not compatible with 'consuming'}} {{none}}
  consuming func fn03() {} // expected-note {{should match modifier here}}

  @abi(nonmutating func fn13()) // expected-error {{default modifier on self parameter in '@abi' is not compatible with 'consuming'}} {{none}}
  consuming func fn13() {} // expected-note {{should match modifier here}}

  @abi(borrowing func fn23()) // expected-error {{modifier 'borrowing' on self parameter in '@abi' is not compatible with 'consuming'}} {{none}}
  consuming func fn23() {} // expected-note {{should match modifier here}}

  @abi(consuming func fn33())
  consuming func fn33() {}

  @abi(__consuming func fn43())
  consuming func fn43() {}

  @abi(mutating func fn53()) // expected-error {{modifier 'mutating' on self parameter in '@abi' is not compatible with 'consuming'}} {{none}}
  consuming func fn53() {} // expected-note {{should match modifier here}}

  @abi(func fn04()) // expected-error {{default modifier on self parameter in '@abi' is not compatible with '__consuming'}} {{none}}
  __consuming func fn04() {} // expected-note {{should match modifier here}}

  @abi(nonmutating func fn14()) // expected-error {{default modifier on self parameter in '@abi' is not compatible with '__consuming'}} {{none}}
  __consuming func fn14() {} // expected-note {{should match modifier here}}

  @abi(borrowing func fn24()) // expected-error {{modifier 'borrowing' on self parameter in '@abi' is not compatible with '__consuming'}} {{none}}
  __consuming func fn24() {} // expected-note {{should match modifier here}}

  @abi(consuming func fn34())
  __consuming func fn34() {}

  @abi(__consuming func fn44())
  __consuming func fn44() {}

  @abi(mutating func fn54()) // expected-error {{modifier 'mutating' on self parameter in '@abi' is not compatible with '__consuming'}} {{none}}
  __consuming func fn54() {} // expected-note {{should match modifier here}}

  @abi(func fn05()) // expected-error {{default modifier on self parameter in '@abi' is not compatible with 'mutating'}} {{none}}
  mutating func fn05() {} // expected-note {{should match modifier here}}

  @abi(nonmutating func fn15()) // expected-error {{default modifier on self parameter in '@abi' is not compatible with 'mutating'}} {{none}}
  mutating func fn15() {} // expected-note {{should match modifier here}}

  @abi(borrowing func fn25()) // expected-error {{modifier 'borrowing' on self parameter in '@abi' is not compatible with 'mutating'}} {{none}}
  mutating func fn25() {} // expected-note {{should match modifier here}}

  @abi(consuming func fn35()) // expected-error {{modifier 'consuming' on self parameter in '@abi' is not compatible with 'mutating'}} {{none}}
  mutating func fn35() {} // expected-note {{should match modifier here}}

  @abi(__consuming func fn45()) // expected-error {{modifier '__consuming' on self parameter in '@abi' is not compatible with 'mutating'}} {{none}}
  mutating func fn45() {} // expected-note {{should match modifier here}}

  @abi(mutating func fn55())
  mutating func fn55() {}
}

// @_addressableSelf -- act like type attribute on `self`
struct AddressableSelf {
  @abi(@_addressableSelf func fn1())
  @_addressableSelf func fn1() {}

  @abi(@_addressableSelf func fn2()) // expected-error {{attribute '_addressableSelf' on self parameter in '@abi' is not compatible with default}} {{none}}
  func fn2() {} // expected-note {{should match attribute here}}

  @abi(func fn3()) // expected-error {{default attribute on self parameter in '@abi' is not compatible with '_addressableSelf'}} {{none}}
  @_addressableSelf func fn3() {} // expected-note {{should match attribute here}}
}

//
// Incorrect usage
//

@abi(func bar()) // expected-note {{attribute already specified here}}
@abi(func foo()) // expected-error {{duplicate attribute}}
func duplicateABIAttr() {}

// Test parser recovery by having something that
// should parse fine.
func somethingThatShouldParseFine() {}

func func_with_nested_abi() {
  @abi(func _exit(_ code: UInt32) -> Void)
  func exit(_ code : UInt32) -> Void {}
  exit(0)
}

@abi(var x = 1) // expected-error {{initial value is not allowed here}} expected-error {{type annotation missing in pattern}}
var x = 1

//
// Examples of expected use cases
//

@abi(func originallySendable() -> @Sendable () -> Void)
func originallySendable() -> sending () -> Void { fatalError() }

@abi(func originallyGenericSendable<T: Sendable>() -> T)
func originallyGenericSendable<T>() -> sending T { fatalError() }

@abi(func originallyAnySendable() -> any Sendable)
func originallyAnySendable() -> sending Any { fatalError() }

@abi(nonisolated func explicitIsolationChanged() -> @Sendable () -> Void)
@MainActor func explicitIsolationChanged() -> sending () -> Void { fatalError() }
