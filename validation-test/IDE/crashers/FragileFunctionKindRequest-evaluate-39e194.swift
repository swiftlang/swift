// {"kind":"complete","original":"2e29372e","signature":"swift::FragileFunctionKindRequest::evaluate(swift::Evaluator&, swift::DeclContext*) const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
if {case ( = { enum a {
#^^#
