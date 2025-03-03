// Constant globals referencing other constant globals and forming a cycle

// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -verify

_const let a: Int = c
_const let b: Int = a
_const let c: Int = b
// expected-error@-1 {{cycle in definitions of constant values}}
