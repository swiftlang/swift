// RUN: %target-typecheck-verify-swift %s \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -disable-objc-interop

// REQUIRES: swift_feature_CxxImplementation


// Malformed arguments are rejected.

// expected-error@+1{{expected C++ function name in 'cxx' attribute}}
@cxx()
func emptyParens() {}

// The name is a bare identifier, not a string literal.
// expected-error@+1{{expected C++ function name in 'cxx' attribute}}
@cxx("cxx_foo")
func stringName() {}

// A label is not accepted.
// expected-error@+1{{expected C++ function name in 'cxx' attribute}}
@cxx(name: "cxx_foo")
func labeledName() {}

// A name that is a Swift keyword must be enclosed in backticks.
// expected-error@+1{{expected C++ function name in 'cxx' attribute}}
@cxx(class)
func keywordName() {}

// C++ operator names must be enclosed in backticks.
// expected-error@+1{{C++ operator name in 'cxx' attribute must be enclosed in backticks}}
@cxx(operator+)
func plainOperatorName(x: Int32) -> Int32 { return x }

// expected-error@+1{{C++ operator name in 'cxx' attribute must be enclosed in backticks}}
@cxx(operator())
func plainCallOperatorName(x: Int32) -> Int32 { return x }


// Only top-level func decls are currently supported.

class Foo {
  // expected-error@+2{{@cxx can only be applied to global functions}}
  // expected-error@+1{{'@cxx' must be combined with '@implementation'}}
  @cxx
  func foo(x: Int32) -> Int32 { return x }

  // expected-error@+2{{@cxx can only be applied to global functions}}
  // expected-error@+1{{'@cxx' must be combined with '@implementation'}}
  @cxx
  static func bar(x: Int32) -> Int32 { return x }
}

// expected-error@+1{{@cxx may only be used on 'func' declarations}}
@cxx
var notAFunction: Int32 = 0

// expected-error@+1{{@cxx may only be used on 'func' declarations}}
@cxx
enum NotAFunctionEnum { case a }

var accessorHost: Int32 {
  // expected-error@+1{{@cxx may only be used on 'func' declarations}}
  @cxx
  get { return 0 }
}


// Reject using both @cxx and @objc on the same decl.

// expected-error@+3{{cannot apply both '@cxx' and '@objc' to global function}}
// expected-error@+2{{'@objc' can only be used with members of classes, '@objc' protocols, and concrete extensions of classes}}
// expected-error@+1{{'@cxx' must be combined with '@implementation'}}
@objc @cxx
func conflictWithObjC(x: Int32) -> Int32 { return x }


// Reject using both @cxx and @c/@_cdecl on the same decl.

// expected-error@+2{{cannot apply both '@cxx' and '@c' to global function}}
// expected-error@+1{{'@cxx' must be combined with '@implementation'}}
@c @cxx
func conflictWithC() {}


// @cxx requires @implementation.

// expected-error@+1{{'@cxx' must be combined with '@implementation'}}
@cxx
func bareCxx(x: Int32) -> Int32 { return x }

// expected-error@+1{{'@cxx' must be combined with '@implementation'}}
@cxx(cxx_foo)
func renamed(x: Int32) -> Int32 { return x }

// expected-error@+1{{'@cxx' must be combined with '@implementation'}}
@cxx(`class`)
func renamedToKeyword(x: Int32) -> Int32 { return x }

// expected-error@+1{{'@cxx' must be combined with '@implementation'}}
@cxx(`operator+`)
func renamedToOperator(x: Int32) -> Int32 { return x }
