// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift -emit-module -o %t %S/Inputs/overload_intFunctions.swift
// RUN: %swift -emit-module -o %t %S/Inputs/overload_boolFunctions.swift
// RUN: %swift -emit-module -o %t %S/Inputs/overload_vars.swift
// RUN: %swift -parse %s -I=%t -sdk= -verify

import overload_intFunctions
import overload_boolFunctions
import overload_vars
import var overload_vars.scopedVar
import func overload_boolFunctions.scopedFunction

struct LocalType {}
func something(obj : LocalType) -> LocalType { return obj }
func something(a : Int, b : Int, c : Int) -> () {}

var _ : Bool = something(true)
var _ : Int = something(1)
var _ : (Int, Int) = something(1, 2)
var _ : LocalType = something(LocalType())
var _ : () = something(1, 2, 3)
something = 42 // expected-error {{expression does not type-check}}

var _ = ambiguousWithVar // expected-error {{expression does not type-check}}
// FIXME: Should be errors -- a variable name and a function cannot overload.
ambiguousWithVar = 42    // no-warning
ambiguousWithVar(true)   // no-warning

var localVar : Bool
localVar = false
localVar = 42 // expected-error {{expression does not type-check}}
localVar(42)  // expected-error {{'$T1 -> $T2' is not identical to 'Bool'}}
var _ : localVar // should still work

var _ = scopedVar // no-warning
scopedVar(42) // expected-error {{'$T1 -> $T2' is not identical to 'Int'}}

var _ : Bool = scopedFunction(true)
var _ : Int  = scopedFunction(42)
scopedFunction = 42 // expected-error {{expression does not type-check}}

// FIXME: Should be an error -- a type name and a function cannot overload.
var _ : Int = TypeNameWins(42)

TypeNameWins = 42 // expected-error {{expression does not type-check}}
var _ : TypeNameWins // no-warning


