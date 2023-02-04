// Test various aspects of the C++ `nodiscard` keyword.

// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop

import NoDiscard

// Test a function.

// CORRECT: return value not discarded.  
var value = NoDiscardAdd(5, 10)

// CORRECT: warning when return value is discarded.
NoDiscardAdd(10, 10) // expected-warning {{result of call to 'NoDiscardAdd' is unused}}


// Test methods in a class.

// CORRECT: return value not discarded.
var multiplier = NoDiscardMultiply()
value = multiplier.Multiply(10, 10)

// CORRECT: warning when return value is discarded
multiplier.Multiply(50, 50)  // expected-warning {{result of call to 'Multiply' is unused}}

// CORRECT: return value not discarded.  
value = multiplier.Divide(100, 10)

// CORRECT: method has no annotation, thus no warning
multiplier.Divide(100, 10)


// Test a struct marked with nodiscard as a return value from a function.

let error: NoDiscardError = NoDiscardReturnError(10, 10)

// INCORRECT: NoDiscardError is declared nodiscard, so ignoring the 
// return value of NoDiscardReturnError() should be a warning, but isn't.
// Filed as: https://github.com/apple/swift/issues/59002
NoDiscardReturnError(50, 50)
