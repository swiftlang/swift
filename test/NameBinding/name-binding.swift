// RUN: %target-swift-frontend -parse %s -module-name themodule -enable-source-import -I %S/../decl -sdk "" -verify -show-diagnostics-after-fatal

import Swift
import nonexistentimport  // expected-error {{no such module 'nonexistentimport'}}

//===----------------------------------------------------------------------===//
// Test imported names
//===----------------------------------------------------------------------===//

// Imported from swift stdlib.
var importedtype : Int

// Imported from enumtest module.
import enum enumtest.unionSearchFlags
var importedunion : unionSearchFlags = .Backwards


// This shouldn't be imported from data.
var notimported : MaybeInt // expected-error {{use of undeclared type 'MaybeInt'}}

//===----------------------------------------------------------------------===//
// Name binding stress test
//===----------------------------------------------------------------------===//

var callee1 : () -> (Int,Int,Int)           // Takes nothing, returns tuple.

func test_shadowing() {
  // Shadow Int.
  enum Int { case xyz; case abc }
  // We get the shadowed version of Int.
  var x : Int = .abc
}

func unknown_member() {
  var error = Swift.nonexistent_member // expected-error {{module 'Swift' has no member named 'nonexistent_member'}}
}

//===----------------------------------------------------------------------===//
// varname Processing
//===----------------------------------------------------------------------===//

var varname1 : (a : Int, b : Int)

// Not very useful, but it is allowed.
var (varname2_a, varname2_b) : (a : Int, b : Int) = varname1


func test_varname_binding() {
  var c = (4, 5)
  var (d, e) = (c.1, c.0)
  var ((), (g1, g2), h) = ((), (e, d), e)
  var (j, k, l) = callee1()
  var (m, n) = callee1() // expected-error{{different number of elements}}
  var (o, p, q, r) = callee1() // expected-error{{different number of elements}}
}

//===----------------------------------------------------------------------===//
// ForwardIndexType referencing of types.
//===----------------------------------------------------------------------===//

// We don't allow namebinding to look forward past a var declaration in the
// main module
var x : x_ty  // expected-error {{use of undeclared type 'x_ty'}}
typealias x_ty = Int

// We allow namebinding to look forward past a function declaration (and other
// declarations which never have side-effects) in the main module
func fy() -> y_ty { return 1 }
typealias y_ty = Int

// FIXME: Should reject this!
//typealias x = x

// FIXME: Should reject this (has infinite size or is tautological depend on
// how you look at it).
enum y {
  case y
  case Int
}

// We don't have a typeof, but this would also be an error somehow.
//var x : typeof(x)


//===----------------------------------------------------------------------===//
// ForwardIndexType referencing of values.
//===----------------------------------------------------------------------===//

func func2() {
  func3()
}

func func3() {
  undefined_func() // expected-error {{use of unresolved identifier 'undefined_func'}}
}

//===----------------------------------------------------------------------===//
// Overloading
//===----------------------------------------------------------------------===//

struct a_struct { var x : Int }

infix operator *** {
  associativity left
  precedence 97
}

func ***(lhs: Int, rhs: Int) -> Int {
  return 4
}
func ***(lhs: a_struct, rhs: a_struct) {}
func ***(lhs: a_struct, rhs: (Int) -> Int) {}


func ov_fn_result() -> Int {}
func ov_fn_result() -> Double {}

func ov_fn_result2() -> (Int) -> (Int) -> Int {}
func ov_fn_result2() -> (Int) -> (a_struct) -> Int {}

func overloadtest(x: Int) {
  var f1 : Int = ((ov_fn_result))()
  var f2 : Double = ((ov_fn_result))()

  // Test overloaded operators.
  var s : a_struct
  4 *** 17     // Resolved to the *** operator that takes ints.
  s *** s     // Resolved to the *** operator that takes a_struct.
  s *** {$0 + 4}     // Closure obviously not a struct.

  ov_fn_result2()(4)(4)  // picks the ov_fn_result2 taking an Int.
  ov_fn_result2()(4)(s)  // picks the ov_fn_result2 taking a_struct.
}

func localtest() {
  func shadowbug() { 
    var Foo = 10
    func g() {
      struct S {
        // FIXME: Swap these two lines to crash our broken lookup.
        typealias Foo = Int
        var x : Foo
      }
    }
  }
  func scopebug() { 
    var Foo = 10
    struct S {
      typealias Foo = Int
    }
  }
  func scopebug2() { 
    struct S1 {}
    struct S2 {
      var x : S1
    }
  }
}

func f0() -> ThisTypeDoesNotExist { return 1 } // expected-error {{use of undeclared type 'ThisTypeDoesNotExist'}}

for _ in [1] { }

//===----------------------------------------------------------------------===//
// Accessing names from our own module
//===----------------------------------------------------------------------===//
var qualifiedvalue : Int = themodule.importedtype
var qualifiedtype : themodule.x_ty = 5


prefix operator +++ {}
postfix operator +++ {}

prefix operator ++ {}
postfix operator ++ {}

prefix func +++(inout a: Int) { a += 2 }
postfix func +++(inout a: Int) { a += 2 }

var test = 0
+++test
test+++


//===----------------------------------------------------------------------===//
// Forward references to local variables.
//===----------------------------------------------------------------------===//

func forwardReference() {
  x = 0 // expected-error{{use of local variable 'x' before its declaration}}
  var x: Float = 0.0 // expected-note{{'x' declared here}}
}

class ForwardReference {
  var x: Int = 0

  func test() {
    x = 0 // expected-error{{use of local variable 'x' before its declaration}}
    var x: Float = 0.0 // expected-note{{'x' declared here}}
  }
}

func questionablyValidForwardReference() { print(qvfrVar); }; var qvfrVar: Int = 0

// FIXME: This should warn too.
print(forwardReferenceVar); var forwardReferenceVar: Int = 0

