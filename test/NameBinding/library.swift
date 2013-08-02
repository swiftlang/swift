// RUN: %swift %s -I=%S/Inputs -parse-as-library -verify

// Name lookup is global in a library.
var x : x_ty
typealias x_ty = Int

// Name lookup is global in a library.
// This case requires doing semantic analysis (conversion checking) after
// parsing.
var y : y_ty = 4
typealias y_ty = (Int)

// Verify that never-defined types are caught.
var z : z_ty  // expected-error {{use of undeclared type 'z_ty'}}

// Make sure we type-check all declarations before any initializers
var a : Int = b
var b : Int = 1

var c : Int
var d : Int
var e : Int

// Name-binding with imports
import imported_module
func over1(x : UInt64) {} // expected-note{{found this candidate}}
func over2(x : UInt32) {}
func over3(x : UInt32) {}
typealias over4 = UInt32
func testover() {
  // FIXME: Very weird diagnostic here.
  over1(0) // expected-error{{ambiguous use of 'over1'}}
  over2(0)
  over3(0) // FIXME: Should we produce an ambiguity error here?
  var x : over4 = 10
}

var x : String.CharEnumeratorType

// FIXME: This testcase should ideally not give an error. Or at least
// give a decent diagnostic.
typealias ZZYZX = String.EnumeratorType // expected-error {{'EnumeratorType' is not a member type of 'String'}}
extension ZZYZX {}

// Referring to name within a module.
var d2 : swift.Int = 5
var y2 : library.y_ty = 5

func increment_y2() {
  y2 = library.y2 + 1
}

