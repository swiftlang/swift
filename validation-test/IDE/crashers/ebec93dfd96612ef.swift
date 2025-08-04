// {"kind":"complete","signature":"(anonymous namespace)::ConstraintWalker::walkToExprPost(swift::Expr*)","signatureAssert":"Assertion failed: (!ty->is<InOutType>() && \"Cannot have InOutType in a tuple\"), function TupleTypeElt"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
-switch  {
case
< (&a
#^^#
