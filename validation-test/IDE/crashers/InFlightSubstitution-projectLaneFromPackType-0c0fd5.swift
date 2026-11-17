// {"kind":"complete","original":"56dd4b7a","signature":"swift::InFlightSubstitution::projectLaneFromPackType(swift::Type, unsigned int)","signatureAssert":"Assertion failed: (index < substPackType->getNumElements() && \"replacement for pack parameter did not have the right \" \"size for expansion\"), function projectLaneFromPackType","signatureNext":"InFlightSubstitution::substType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<each b{
init(c: repeat each b^func d<each e>(
f: repeat each b -> each e
}
a(repeat ... each c)#^^#
