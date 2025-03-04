// Constant globals referencing other constant globals and forming a cycle
// REQUIRES: swift_feature_CompileTimeValues
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -verify -enable-experimental-feature CompileTimeValues

@const let a: Int = c
@const let b: Int = a
@const let c: Int = b
// expected-error@-1 {{cycle in definitions of constant values}}
