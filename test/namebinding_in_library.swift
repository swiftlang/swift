// RUN: %swift %s -parse-as-library -verify

// Name lookup is global in a library.
var x : x_ty
typealias x_ty : Int

// Name lookup is global in a library.
// This case requires doing semantic analysis (conversion checking) after
// parsing.
var y : y_ty = 4
typealias y_ty : (Int)

// Verify that never-defined types are caught.
var z : z_ty  // expected-error {{use of undeclared type 'z_ty'}}
