// {"kind":"typecheck","signature":"swift::PatternTypeRequest::evaluate(swift::Evaluator&, swift::ContextualPattern) const","signatureAssert":"Assertion failed: (!dyn_cast_or_null<SpecifierTypeRepr>(Repr) && \"Didn't resolve invalid type to error type!\"), function validateTypedPattern"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a { b : _const a
