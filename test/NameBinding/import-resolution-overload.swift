// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/overload_intFunctions.swift
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/overload_boolFunctions.swift
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/overload_vars.swift
// RUN: %target-swift-frontend -parse %s -I %t -sdk "" -verify

import overload_intFunctions
import overload_boolFunctions
import overload_vars
import var overload_vars.scopedVar
import func overload_boolFunctions.scopedFunction

struct LocalType {}
func something(obj: LocalType) -> LocalType { return obj }
func something(a: Int, b: Int, c: Int) -> () {}

var _ : Bool = something(true)
var _ : Int = something(1)
var _ : (Int, Int) = something(1, 2)
var _ : LocalType = something(LocalType())
var _ : () = something(1, 2, 3)
something = 42 // expected-error {{could not find an overload for 'something' that accepts the supplied arguments}}

let ambValue = ambiguousWithVar // no-warning - var preferred
let ambValueChecked: Int = ambValue
ambiguousWithVar = 42    // no-warning
ambiguousWithVar(true)   // no-warning

var localVar : Bool
localVar = false
localVar = 42 // expected-error {{cannot assign a value of type 'Int' to a value of type 'Bool'}}
localVar(42)  // expected-error {{cannot invoke 'localVar' with an argument list of type '(Int)'}}
var _ : localVar // should still work

var _ = scopedVar // no-warning
scopedVar(42) // expected-error {{cannot invoke 'scopedVar' with an argument list of type '(Int)'}}

var _ : Bool = scopedFunction(true)
var _ : Int  = scopedFunction(42)
scopedFunction = 42 // expected-error {{ambiguous use of 'scopedFunction'}}

// FIXME: Should be an error -- a type name and a function cannot overload.
var _ : Int = TypeNameWins(42)

TypeNameWins = 42 // expected-error {{ambiguous use of 'TypeNameWins'}}
var _ : TypeNameWins // no-warning


