// The CompileTimeValuesPreview-only regime. Preview disables all AST-level
// syntactic checking (LegalLiteralExprVerifier.cpp returns early), so
// hasSingleVarConstantFoldedInit's guard is the only thing keeping the
// literal-expression folder off these initializers. '@const' needs
// CompileTimeValues, so this file uses '@section' only.
//
// Both runs share one set of expectations: enabling LiteralExpressions must not
// perturb the Preview regime.
// REQUIRES: swift_feature_CompileTimeValuesPreview
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -verify -enable-experimental-feature CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -verify -enable-experimental-feature CompileTimeValuesPreview -enable-experimental-feature LiteralExpressions

// A compile-time value that is not a literal expression. The folder would
// reject it; nothing here may.
@section("mysection") let sectionFromFloatConversion: Int = Int(17.0 / 3.5)

@section("mysection") let sectionIntegerArithmetic: Int = 2 * 4096
let plainGlobal = 7
@section("mysection") let sectionVariableReference: Int = plainGlobal + 1
