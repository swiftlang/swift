// {"kind":"typecheck","signature":"swift::PatternTypeRequest::evaluate(swift::Evaluator&, swift::ContextualPattern) const","signatureAssert":"Assertion failed: (!dyn_cast_or_null<SpecifierTypeRepr>(Repr) && \"Didn't resolve invalid type to error type!\"), function validateTypedPattern","signatureNext":"SimpleRequest::evaluateRequest"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a { b : _const a
