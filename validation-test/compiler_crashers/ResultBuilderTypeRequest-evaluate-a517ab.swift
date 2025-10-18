// {"kind":"typecheck","signature":"swift::ResultBuilderTypeRequest::evaluate(swift::Evaluator&, swift::ValueDecl*) const","signatureAssert":"Assertion failed: (!hasTypeParameter() && \"already have an interface type\"), function mapTypeOutOfContext"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@resultBuilder enum a < b { struct c{@a d:
