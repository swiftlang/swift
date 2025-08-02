// {"kind":"typecheck","signature":"swift::ResultBuilderTypeRequest::evaluate(swift::Evaluator&, swift::ValueDecl*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@resultBuilder enum a < b { struct c{@a d:
