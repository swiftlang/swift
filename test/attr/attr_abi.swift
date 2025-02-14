// RUN: %target-typecheck-verify-swift -enable-experimental-feature Extern -enable-experimental-feature ABIAttribute -enable-experimental-feature AddressableParameters -parse-as-library

// REQUIRES: swift_feature_ABIAttribute
// REQUIRES: swift_feature_Extern
// REQUIRES: swift_feature_AddressableParameters

import _Differentiation

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

//
// Throws effect checking
//

@abi(func throws00(_: () throws -> Void))
func throws00(_: () throws -> Void) {}

@abi(func throws10(_: () throws -> Void) throws) // expected-error {{cannot give 'throws10' the ABI of a global function which can throw}}
func throws10(_: () throws -> Void) {}

@abi(func throws20(_: () throws -> Void) rethrows) // expected-error {{cannot give 'throws20' the ABI of a global function which can throw}}
func throws20(_: () throws -> Void) {}

@abi(func throws01(_: () throws -> Void)) // expected-error {{cannot give 'throws01' the ABI of a global function which cannot throw}}
func throws01(_: () throws -> Void) throws {}

@abi(func throws11(_: () throws -> Void) throws)
func throws11(_: () throws -> Void) throws {}

@abi(func throws21(_: () throws -> Void) rethrows)
func throws21(_: () throws -> Void) throws {}

@abi(func throws02(_: () throws -> Void)) // expected-error {{cannot give 'throws02' the ABI of a global function which cannot throw}}
func throws02(_: () throws -> Void) rethrows {}

@abi(func throws12(_: () throws -> Void) throws)
func throws12(_: () throws -> Void) rethrows {}

@abi(func throws22(_: () throws -> Void) rethrows)
func throws22(_: () throws -> Void) rethrows {}

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

//
// Miscellaneous function checking
//

@_silgen_name("conflictingAttrsSilgenName")
@abi(func conflictingAttrsABI())
func conflictingAttrsAPI() {} // expected-error@-2 {{cannot use '@_silgen_name' and '@abi' on the same global function because they serve the same purpose}} {{1-44=}}

//
// PBD shape checking
//

@abi(var x1, y1: Int) // expected-error {{cannot give pattern binding the ABI of a binding with more patterns}}
var x1: Int = 0

@abi(var x2: Int)
var x2 = 0, y2: Int = 0 // expected-error {{cannot give pattern binding the ABI of a binding with fewer patterns}}

@abi(var (x3, y3): (Int, Int), (a3, b3): (Int, Int)) // expected-error {{no match for ABI var 'b3'}}
var (x3, y3): (Int, Int) = (0, 0), a3: Int = 0

@abi(var (x4, y4): (Int, Int), a4: Int)
var (x4, y4): (Int, Int) = (0, 0), (a4, b4): (Int, Int) = (0, 0) // expected-error {{no match for var 'b4' in the ABI}}

//
// Conflict diagnostics
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

@abi(func floatForIntParam(_: Float) -> Int) // expected-error @:31 {{type 'Float' in '@abi' should match 'Int'}}
func intForFloatParam(_: Int) -> Int { fatalError() } // expected-note @:26 {{should match type here}}

@abi(func floatForIntResult(_: Int) -> Float) // expected-error @:40 {{type 'Float' in '@abi' should match 'Int'}}
func intForFloatResult(_: Int) -> Int { fatalError() } // expected-note @:35 {{should match type here}}

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

@abi(func arrayForVariadicParam(a: [Int], b: Set<Float>)) // expected-error @:46 {{type 'Set<Float>' in '@abi' should match 'Float...'}}
func arrayForVariadicParam(a: Int..., b: Float...) {} // expected-note @:42 {{should match type here}}

struct DefaultParamOwnership {
  @abi(
    func method(
      _: AnyObject,
      _: inout AnyObject, // expected-error {{parameter modifier 'inout' in '@abi' is not compatible with default}}
      _: borrowing AnyObject,
      _: consuming AnyObject, // expected-error {{parameter modifier 'consuming' in '@abi' is not compatible with default}}
      _: __shared AnyObject,
      _: __owned AnyObject, // expected-error {{parameter modifier '__owned' in '@abi' is not compatible with default}}
      _: sending AnyObject, // expected-error {{parameter modifier 'sending' in '@abi' is not compatible with default}}
      _: (AnyObject) -> Void,
      _: (borrowing AnyObject) -> Void,
      _: (consuming AnyObject) -> Void // expected-error {{type '(consuming AnyObject) -> Void' in '@abi' should match '(AnyObject) -> Void'}}
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
      _: AnyObject,
      _: inout AnyObject, // expected-error {{parameter modifier 'inout' in '@abi' is not compatible with default}}
      _: borrowing AnyObject, // expected-error {{parameter modifier 'borrowing' in '@abi' is not compatible with default}}
      _: consuming AnyObject,
      _: __shared AnyObject, // expected-error {{parameter modifier '__shared' in '@abi' is not compatible with default}}
      _: __owned AnyObject,
      _: sending AnyObject,
      _: (AnyObject) -> Void,
      _: (borrowing AnyObject) -> Void,
      _: (consuming AnyObject) -> Void // expected-error {{type '(consuming AnyObject) -> Void' in '@abi' should match '(AnyObject) -> Void'}}
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
      _: AnyObject, // expected-error {{default parameter modifier in '@abi' is not compatible with 'inout'}}
      _: inout AnyObject,
      _: borrowing AnyObject, // expected-error {{parameter modifier 'borrowing' in '@abi' is not compatible with 'inout'}}
      _: consuming AnyObject, // expected-error {{parameter modifier 'consuming' in '@abi' is not compatible with 'inout'}}
      _: __shared AnyObject, // expected-error {{parameter modifier '__shared' in '@abi' is not compatible with 'inout'}}
      _: __owned AnyObject, // expected-error {{parameter modifier '__owned' in '@abi' is not compatible with 'inout'}}
      _: sending AnyObject, // expected-error {{parameter modifier 'sending' in '@abi' is not compatible with 'inout'}}
      _: (AnyObject) -> Void, // expected-error {{type '(AnyObject) -> Void' in '@abi' should match '(inout AnyObject) -> Void'}}
      _: (borrowing AnyObject) -> Void, // expected-error {{type '(borrowing AnyObject) -> Void' in '@abi' should match '(inout AnyObject) -> Void'}}
      _: (consuming AnyObject) -> Void // expected-error {{type '(consuming AnyObject) -> Void' in '@abi' should match '(inout AnyObject) -> Void'}}
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
      _: AnyObject, // expected-error {{default parameter modifier in '@abi' is not compatible with 'inout'}}
      _: inout AnyObject,
      _: borrowing AnyObject, // expected-error {{parameter modifier 'borrowing' in '@abi' is not compatible with 'inout'}}
      _: consuming AnyObject, // expected-error {{parameter modifier 'consuming' in '@abi' is not compatible with 'inout'}}
      _: __shared AnyObject, // expected-error {{parameter modifier '__shared' in '@abi' is not compatible with 'inout'}}
      _: __owned AnyObject, // expected-error {{parameter modifier '__owned' in '@abi' is not compatible with 'inout'}}
      _: sending AnyObject, // expected-error {{parameter modifier 'sending' in '@abi' is not compatible with 'inout'}}
      _: (AnyObject) -> Void, // expected-error {{type '(AnyObject) -> Void' in '@abi' should match '(inout AnyObject) -> Void'}}
      _: (borrowing AnyObject) -> Void, // expected-error {{type '(borrowing AnyObject) -> Void' in '@abi' should match '(inout AnyObject) -> Void'}}
      _: (consuming AnyObject) -> Void // expected-error {{type '(consuming AnyObject) -> Void' in '@abi' should match '(inout AnyObject) -> Void'}}
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
      _: AnyObject,
      _: inout AnyObject, // expected-error {{parameter modifier 'inout' in '@abi' is not compatible with 'borrowing'}}
      _: borrowing AnyObject,
      _: consuming AnyObject, // expected-error {{parameter modifier 'consuming' in '@abi' is not compatible with 'borrowing'}}
      _: __shared AnyObject,
      _: __owned AnyObject, // expected-error {{parameter modifier '__owned' in '@abi' is not compatible with 'borrowing'}}
      _: sending AnyObject, // expected-error {{parameter modifier 'sending' in '@abi' is not compatible with 'borrowing'}}
      _: (AnyObject) -> Void,
      _: (borrowing AnyObject) -> Void,
      _: (consuming AnyObject) -> Void // expected-error {{type '(consuming AnyObject) -> Void' in '@abi' should match '(borrowing AnyObject) -> Void'}}
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
      _: AnyObject, // expected-error {{default parameter modifier in '@abi' is not compatible with 'borrowing'}}
      _: inout AnyObject, // expected-error {{parameter modifier 'inout' in '@abi' is not compatible with 'borrowing'}}
      _: borrowing AnyObject,
      _: consuming AnyObject, // expected-error {{parameter modifier 'consuming' in '@abi' is not compatible with 'borrowing'}}
      _: __shared AnyObject,
      _: __owned AnyObject, // expected-error {{parameter modifier '__owned' in '@abi' is not compatible with 'borrowing'}}
      _: sending AnyObject, // expected-error {{parameter modifier 'sending' in '@abi' is not compatible with 'borrowing'}}
      _: (AnyObject) -> Void,
      _: (borrowing AnyObject) -> Void,
      _: (consuming AnyObject) -> Void // expected-error {{type '(consuming AnyObject) -> Void' in '@abi' should match '(borrowing AnyObject) -> Void'}}
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
      _: AnyObject, // expected-error {{default parameter modifier in '@abi' is not compatible with 'consuming'}}
      _: inout AnyObject, // expected-error {{parameter modifier 'inout' in '@abi' is not compatible with 'consuming'}}
      _: borrowing AnyObject, // expected-error {{parameter modifier 'borrowing' in '@abi' is not compatible with 'consuming'}}
      _: consuming AnyObject,
      _: __shared AnyObject, // expected-error {{parameter modifier '__shared' in '@abi' is not compatible with 'consuming'}}
      _: __owned AnyObject,
      _: sending AnyObject,
      _: (AnyObject) -> Void, // expected-error {{type '(AnyObject) -> Void' in '@abi' should match '(consuming AnyObject) -> Void'}}
      _: (borrowing AnyObject) -> Void, // expected-error {{type '(borrowing AnyObject) -> Void' in '@abi' should match '(consuming AnyObject) -> Void'}}
      _: (consuming AnyObject) -> Void
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
      _: AnyObject,
      _: inout AnyObject, // expected-error {{parameter modifier 'inout' in '@abi' is not compatible with 'consuming'}}
      _: borrowing AnyObject, // expected-error {{parameter modifier 'borrowing' in '@abi' is not compatible with 'consuming'}}
      _: consuming AnyObject,
      _: __shared AnyObject, // expected-error {{parameter modifier '__shared' in '@abi' is not compatible with 'consuming'}}
      _: __owned AnyObject,
      _: sending AnyObject,
      _: (AnyObject) -> Void, // expected-error {{type '(AnyObject) -> Void' in '@abi' should match '(consuming AnyObject) -> Void'}}
      _: (borrowing AnyObject) -> Void, // expected-error {{type '(borrowing AnyObject) -> Void' in '@abi' should match '(consuming AnyObject) -> Void'}}
      _: (consuming AnyObject) -> Void
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
      _: AnyObject,
      _: inout AnyObject, // expected-error {{parameter modifier 'inout' in '@abi' is not compatible with '__shared'}}
      _: borrowing AnyObject,
      _: consuming AnyObject, // expected-error {{parameter modifier 'consuming' in '@abi' is not compatible with '__shared'}}
      _: __shared AnyObject,
      _: __owned AnyObject, // expected-error {{parameter modifier '__owned' in '@abi' is not compatible with '__shared'}}
      _: sending AnyObject, // expected-error {{parameter modifier 'sending' in '@abi' is not compatible with '__shared'}}
      _: (AnyObject) -> Void,
      _: (borrowing AnyObject) -> Void,
      _: (consuming AnyObject) -> Void // expected-error {{type '(consuming AnyObject) -> Void' in '@abi' should match '(__shared AnyObject) -> Void'}}
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
      _: AnyObject, // expected-error {{default parameter modifier in '@abi' is not compatible with '__shared'}}
      _: inout AnyObject, // expected-error {{parameter modifier 'inout' in '@abi' is not compatible with '__shared'}}
      _: borrowing AnyObject,
      _: consuming AnyObject, // expected-error {{parameter modifier 'consuming' in '@abi' is not compatible with '__shared'}}
      _: __shared AnyObject,
      _: __owned AnyObject, // expected-error {{parameter modifier '__owned' in '@abi' is not compatible with '__shared'}}
      _: sending AnyObject, // expected-error {{parameter modifier 'sending' in '@abi' is not compatible with '__shared'}}
      _: (AnyObject) -> Void,
      _: (borrowing AnyObject) -> Void,
      _: (consuming AnyObject) -> Void // expected-error {{type '(consuming AnyObject) -> Void' in '@abi' should match '(__shared AnyObject) -> Void'}}
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
      _: AnyObject, // expected-error {{default parameter modifier in '@abi' is not compatible with '__owned'}}
      _: inout AnyObject, // expected-error {{parameter modifier 'inout' in '@abi' is not compatible with '__owned'}}
      _: borrowing AnyObject, // expected-error {{parameter modifier 'borrowing' in '@abi' is not compatible with '__owned'}}
      _: consuming AnyObject,
      _: __shared AnyObject, // expected-error {{parameter modifier '__shared' in '@abi' is not compatible with '__owned'}}
      _: __owned AnyObject,
      _: sending AnyObject,
      _: (AnyObject) -> Void, // expected-error {{type '(AnyObject) -> Void' in '@abi' should match '(__owned AnyObject) -> Void'}}
      _: (borrowing AnyObject) -> Void, // expected-error {{type '(borrowing AnyObject) -> Void' in '@abi' should match '(__owned AnyObject) -> Void'}}
      _: (consuming AnyObject) -> Void
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
      _: AnyObject,
      _: inout AnyObject, // expected-error {{parameter modifier 'inout' in '@abi' is not compatible with '__owned'}}
      _: borrowing AnyObject, // expected-error {{parameter modifier 'borrowing' in '@abi' is not compatible with '__owned'}}
      _: consuming AnyObject,
      _: __shared AnyObject, // expected-error {{parameter modifier '__shared' in '@abi' is not compatible with '__owned'}}
      _: __owned AnyObject,
      _: sending AnyObject,
      _: (AnyObject) -> Void, // expected-error {{type '(AnyObject) -> Void' in '@abi' should match '(__owned AnyObject) -> Void'}}
      _: (borrowing AnyObject) -> Void, // expected-error {{type '(borrowing AnyObject) -> Void' in '@abi' should match '(__owned AnyObject) -> Void'}}
      _: (consuming AnyObject) -> Void
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
      _: AnyObject, // expected-error {{default parameter modifier in '@abi' is not compatible with 'sending'}}
      _: inout AnyObject, // expected-error {{parameter modifier 'inout' in '@abi' is not compatible with 'sending'}}
      _: borrowing AnyObject, // expected-error {{parameter modifier 'borrowing' in '@abi' is not compatible with 'sending'}}
      _: consuming AnyObject,
      _: __shared AnyObject, // expected-error {{parameter modifier '__shared' in '@abi' is not compatible with 'sending'}}
      _: __owned AnyObject,
      _: sending AnyObject,
      _: (AnyObject) -> Void, // expected-error {{type '(AnyObject) -> Void' in '@abi' should match '(sending AnyObject) -> Void'}}
      _: (borrowing AnyObject) -> Void, // expected-error {{type '(borrowing AnyObject) -> Void' in '@abi' should match '(sending AnyObject) -> Void'}}
      _: (consuming AnyObject) -> Void
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
      _: AnyObject,
      _: inout AnyObject, // expected-error {{parameter modifier 'inout' in '@abi' is not compatible with 'sending'}}
      _: borrowing AnyObject, // expected-error {{parameter modifier 'borrowing' in '@abi' is not compatible with 'sending'}}
      _: consuming AnyObject,
      _: __shared AnyObject, // expected-error {{parameter modifier '__shared' in '@abi' is not compatible with 'sending'}}
      _: __owned AnyObject,
      _: sending AnyObject,
      _: (AnyObject) -> Void, // expected-error {{type '(AnyObject) -> Void' in '@abi' should match '(sending AnyObject) -> Void'}}
      _: (borrowing AnyObject) -> Void, // expected-error {{type '(borrowing AnyObject) -> Void' in '@abi' should match '(sending AnyObject) -> Void'}}
      _: (consuming AnyObject) -> Void
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
@abi(
  func nonEphemeralTest(
    @_nonEphemeral _: UnsafeRawPointer,
    _: UnsafeRawPointer,
    @_nonEphemeral _: UnsafeRawPointer,
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
@abi(func noDerivativeTest1(_: @differentiable(reverse) (@noDerivative Double, Double) -> Double))
func noDerivativeTest1(_: @differentiable(reverse) (@noDerivative Double, Double) -> Double) {}

@abi(func noDerivativeTest2(_: @differentiable(reverse) (Double, Double) -> Double)) // expected-error {{type '@differentiable(reverse) (Double, Double) -> Double' in '@abi' should match '@differentiable(reverse) (@noDerivative Double, Double) -> Double'}}
func noDerivativeTest2(_: @differentiable(reverse) (@noDerivative Double, Double) -> Double) {} // expected-note {{should match type here}}

@abi(func noDerivativeTest3(_: @differentiable(reverse) (@noDerivative Double, Double) -> Double)) // expected-error {{type '@differentiable(reverse) (@noDerivative Double, Double) -> Double' in '@abi' should match '@differentiable(reverse) (Double, Double) -> Double'}}
func noDerivativeTest3(_: @differentiable(reverse) (Double, Double) -> Double) {} // expected-note {{should match type here}}

// @_addressable should match
@abi(
  func addressableTest(
    _: @_addressable String,
    _: String, // expected-error {{default type attribute in '@abi' is not compatible with '_addressable'}}
    _: @_addressable String, // expected-error {{type attribute '_addressable' in '@abi' is not compatible with default}}
    _: String
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
    _: @escaping () -> AnyObject,
    _: @Sendable () -> AnyObject,
    _: () -> sending AnyObject,
    _: () -> AnyObject,
    _: @MainActor () -> AnyObject,
    _: (isolated MainActor) -> AnyObject,
    _: @isolated(any) () -> AnyObject, // expected-error {{type '@isolated(any) () -> AnyObject' in '@abi' should match '() -> AnyObject'}}
    _: @execution(caller) () async -> AnyObject,
    _: () -> AnyObject, // expected-error {{type '() -> AnyObject' in '@abi' should match '@isolated(any) () -> AnyObject'}}
    _: () async -> Void,
    _: () -> Void, // expected-error {{type '() -> Void' in '@abi' should match '() async -> Void'}}
    _: () async -> Void, // expected-error {{type '() async -> Void' in '@abi' should match '() -> Void'}}
    _: () -> Void,
    _: () throws -> Void,
    _: () -> Void, // expected-error {{type '() -> Void' in '@abi' should match '() throws -> Void'}}
    _: () throws -> Void, // expected-error {{type '() throws -> Void' in '@abi' should match '() -> Void'}}
    _: () -> Void,
    _: () -> Void,
    _: @convention(block) () -> Void, // expected-error {{type '@convention(block) () -> Void' in '@abi' should match '() -> Void'}}
    _: @convention(thin) () -> Void, // expected-error {{type '@convention(thin) () -> Void' in '@abi' should match '() -> Void'}}
    _: @convention(c) () -> Void // expected-error {{type '@convention(c) () -> Void' in '@abi' should match '() -> Void'}}
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
  func testMarkerProtocols<A, B: Sendable>(
    _: A, _: B,
    _: Any, _: Sendable,
    _: AnyKeyPath, _: AnyKeyPath & Sendable,
    _: Any, _: Sendable & BitwiseCopyable
  )
)
func testMarkerProtocols<A: Sendable, B>(
  _: A, _: B,
  _: Sendable, _: Any,
  _: AnyKeyPath & Sendable, _: AnyKeyPath,
  _: Sendable & BitwiseCopyable, _: Any
) {}

@abi(
  func testNormalProtocols(
    _: Any, // expected-error {{type 'Any' in '@abi' should match 'any CustomStringConvertible'}}
    _: CustomStringConvertible, // expected-error {{type 'any CustomStringConvertible' in '@abi' should match 'Any'}}
    _: AnyKeyPath, // expected-error {{type 'AnyKeyPath' in '@abi' should match 'any AnyKeyPath & CustomStringConvertible'}}
    _: AnyKeyPath & CustomStringConvertible, // expected-error {{type 'any AnyKeyPath & CustomStringConvertible' in '@abi' should match 'AnyKeyPath'}}
    _: Any, // expected-error {{type 'Any' in '@abi' should match 'any CustomDebugStringConvertible & CustomStringConvertible'}}
    _: CustomStringConvertible & CustomDebugStringConvertible // expected-error {{type 'any CustomDebugStringConvertible & CustomStringConvertible' in '@abi' should match 'Any'}}
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
