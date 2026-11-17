// {"kind":"complete","original":"02ad15e2","signature":"swift::InFlightSubstitution::projectLaneFromPackType(swift::Type, unsigned int)","signatureAssert":"Assertion failed: (!substEltType->is<PackExpansionType>() && \"substituted shape mismatch: expected a scalar component\"), function projectLaneFromPackType","signatureNext":"InFlightSubstitution::substType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<each b{
init(repeat each b)
c<each d> (repeat (each b, each d))
}
let e = a(0.0let f = e#^^#
