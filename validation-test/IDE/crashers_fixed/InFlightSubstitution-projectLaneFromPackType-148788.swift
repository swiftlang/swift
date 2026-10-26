// {"kind":"complete","original":"6c98e1c8","signature":"swift::InFlightSubstitution::projectLaneFromPackType(swift::Type, unsigned int)","signatureAssert":"Assertion failed: (index < substPackType->getNumElements() && \"replacement for pack parameter did not have the right \" \"size for expansion\"), function projectLaneFromPackType","signatureNext":"InFlightSubstitution::substType"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a < each b {
struct c < each d {
struct e {
typealias f<each g, each h> = (repeat (each g each h
typealias i = f<repeat each b, repeat each d>
j = a<Int, String>.c<Bool>.e#^^#
