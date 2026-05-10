// Test that typed and untyped enums cannot be assigned to each other. 

// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop

import TypedUntypedEnums

// Implicit type. |x| is an |Int|.
let x = kThree

// Implicit type, |myColor| is a |Color|.
var myColor = kYellow

// These correctly fail. Cannot convert |Int| <-> |Color| (an enum).
myColor = kTwo // expected-error {{cannot assign value of type 'Int' to type 'Color'}}
myColor = x // expected-error {{cannot assign value of type 'Int' to type 'Color'}}
let integer : Int = kBlue // expected-error {{cannot convert value of type 'Color' to specified type 'Int'}}

// These correctly fail. Cannot convert |Int| <-> |Pet| (an enum class).
let animal : Pet = 7 // expected-error {{cannot convert value of type 'Int' to specified type 'Pet'}}
let number : Int = Pet.goat // expected-error {{cannot convert value of type 'Pet' to specified type 'Int'}}
