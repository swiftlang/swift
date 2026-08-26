// The compile-time-values evaluator owns '@const'/'@section' initializers.
// Enabling LiteralExpressions on top of it must change nothing: the AST-level
// literal-expression folder accepts a strictly narrower grammar, so folding here
// would reject a valid compile-time value, and that error would set
// ASTContext::hadError() and suppress the SIL passes' own diagnostic.
//
// Each regime runs twice, with and without LiteralExpressions, against one set
// of expectations.
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: swift_feature_CompileTimeValuesPreview
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -verify -verify-additional-prefix preview- -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -verify -verify-additional-prefix preview- -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview -enable-experimental-feature LiteralExpressions
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -verify -verify-additional-prefix const- -enable-experimental-feature CompileTimeValues
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -verify -verify-additional-prefix const- -enable-experimental-feature CompileTimeValues -enable-experimental-feature LiteralExpressions

// A compile-time value that is not a literal expression. Accepted in both
// regimes; the folder must not reject it.
@const let constFromFloatConversion: Int = Int(17.0 / 3.5)

@const let constIntegerArithmetic: Int = 2 * 4096

// Not a compile-time value. CompileTimeValuesPreview does no syntactic
// checking, so the diagnostic comes from DiagnoseUnknownConstValues, which bails
// on any earlier error -- if the folder diagnoses first, this expectation fails.
@const let constNotCompileTime: Int = Int.random(in: 0..<10)
// expected-preview-error@-1 {{'@const' value should be initialized with a compile-time value}}
// expected-const-error@-2 {{not supported in a literal expression}}

// '@section' integer initializers are statically initialized by the SIL
// pipeline without the AST-level fold.
@section("mysection") let sectionIntegerArithmetic: Int = 2 * 4096
let plainGlobal = 7
@section("mysection") let sectionVariableReference: Int = plainGlobal + 1
