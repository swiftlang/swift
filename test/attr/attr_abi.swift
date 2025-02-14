// RUN: %target-typecheck-verify-swift -enable-experimental-feature Extern -enable-experimental-feature ABIAttribute -enable-experimental-feature AddressableParameters -enable-experimental-feature NoImplicitCopy -enable-experimental-feature SymbolLinkageMarkers -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature CImplementation -import-bridging-header %S/Inputs/attr_abi.h -parse-as-library -Rabi-inference -debugger-support

// REQUIRES: swift_feature_ABIAttribute
// REQUIRES: swift_feature_Extern
// REQUIRES: swift_feature_AddressableParameters
// REQUIRES: swift_feature_NoImplicitCopy
// REQUIRES: swift_feature_SymbolLinkageMarkers
// REQUIRES: swift_feature_AllowUnsafeAttribute
// REQUIRES: swift_feature_CImplementation

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

@abi(var throws00Var: Int)
var throws00Var: Int { get { fatalError() } }

@abi(var throws11Var: Int)
var throws11Var: Int { get throws { fatalError() } }

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
// Attributes
//

// @_originallyDefinedIn intentionally allowed to vary
@abi(@_originallyDefinedIn(module: "Other", macOS 14) func originallyDefinedIn1())
@available(macOS 12, *) @_originallyDefinedIn(module: "Other", macOS 14) public func originallyDefinedIn1() {}

@abi(func originallyDefinedIn2())
@available(macOS 12, *) @_originallyDefinedIn(module: "Other", macOS 14) public func originallyDefinedIn2() {}

@abi(@_originallyDefinedIn(module: "Other", macOS 14) func originallyDefinedIn3())
@available(macOS 12, *) public func originallyDefinedIn3() {}

@abi(@_originallyDefinedIn(module: "Different", macOS 12) func originallyDefinedIn4())
@available(macOS 12, *) @_originallyDefinedIn(module: "Other", macOS 14) public func originallyDefinedIn4() {}

// @Sendable
@abi(@Sendable func sendable1())
@Sendable func sendable1() {}

@abi(@Sendable func sendable2())
func sendable2() {}

@abi(func sendable3())
@Sendable func sendable3() {}

// @preconcurrency
@abi(@preconcurrency func preconcurrency1())
@preconcurrency func preconcurrency1() {}

@abi(@preconcurrency func preconcurrency2())
func preconcurrency2() {}

@abi(func preconcurrency3())
@preconcurrency func preconcurrency3() {}

// @_preInverseGenerics
struct PreInverseGenerics<T: ~Copyable> {
  @abi(@_preInverseGenerics func fn1(_: consuming T))
  @_preInverseGenerics func fn1(_: consuming T) {}

  @abi(@_preInverseGenerics func fn2(_: consuming T))
  func fn2(_: consuming T) {}

  @abi(func fn3(_: consuming T))
  @_preInverseGenerics func fn3(_: consuming T) {}
}

// 'nonisolated', 'isolated' arguments, global actors
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

// @_inheritActorContext -- banned in @abi
@abi(func inheritActorContext1(@_inheritActorContext fn: @Sendable @escaping () async -> Void)) // expected-error {{unused '_inheritActorContext' attribute in '@abi'}} {{32-53=}}
func inheritActorContext1(@_inheritActorContext fn: @Sendable @escaping () async -> Void) {}

@abi(func inheritActorContext2(@_inheritActorContext fn: @Sendable @escaping () async -> Void)) // expected-error {{unused '_inheritActorContext' attribute in '@abi'}} {{32-53=}}
func inheritActorContext2(fn: @Sendable @escaping () async -> Void) {}

@abi(func inheritActorContext3(fn: @Sendable @escaping () async -> Void))
func inheritActorContext3(@_inheritActorContext fn: @Sendable @escaping () async -> Void) {}

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

// Access control, @usableFromInline, @_spi -- inherited at the request level
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

// @available, @_unavailable*, @backDeployed -- inherited at the request level

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

// override, @_nonoverride -- inherited at the request level
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

// @_silgen_name can't be combined with @abi
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
@abi(@_cdecl("cdecl1") func cdecl1()) // expected-error {{unused '_cdecl' attribute in '@abi'}} {{6-23=}}
@_cdecl("cdecl1") func cdecl1() {}

@abi(@_cdecl("cdecl2") func cdecl2()) // expected-error {{unused '_cdecl' attribute in '@abi'}} {{6-23=}}
func cdecl2() {}

@abi(func cdecl3())
@_cdecl("cdecl3") func cdecl3() {}

// @implementation -- banned in @abi
@abi(@implementation func implementation1()) // expected-error {{unused 'implementation' attribute in '@abi'}} {{6-21=}}
@_cdecl("implementation1") @implementation func implementation1() {}

@abi(@implementation func implementation2()) // expected-error {{unused 'implementation' attribute in '@abi'}} {{6-21=}}
@_cdecl("implementation2") func implementation2() {}

@abi(func implementation3())
@_cdecl("implementation3") @implementation func implementation3() {}









// `@_borrowed` cloned onto child
protocol BorrowedAttr {
  @abi(@_borrowed var v1: Int)
  @_borrowed var v1: Int { get set }

  @abi(var v2: Int) // expected-remark {{inferred '@_borrowed' in '@abi' to match attribute on API}}
  @_borrowed var v2: Int { get set } // expected-note {{matches attribute here}}

  @abi(@_borrowed var v3: Int) // expected-error {{extra '_borrowed' attribute in '@abi'}} {{8-18=}}
  var v3: Int { get set }
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
