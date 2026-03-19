// {"kind":"complete","original":"0cd48ae7","signature":"swift::TypeBase::getReducedShape()","signatureAssert":"Assertion failed: (!isTypeVariableOrMember()), function getReducedShape","signatureNext":"ConstraintSystem::simplifySameShapeConstraint"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<each b { c: (repeat each b )! { c #^^#
