// {"kind":"complete","original":"a23c3773","signature":"checkRequirementsImpl(llvm::ArrayRef<swift::Requirement>, bool)","signatureAssert":"Assertion failed: ((allowTypeParameters || !secondType->hasTypeParameter()) && \"must take a contextual type. if you really are ok with an \" \"indefinite answer (and usually YOU ARE NOT), then consider whether \" \"you really, definitely are ok with an indefinite answer, and \" \"use `checkRequirementsWithoutContext` instead\"), function checkRequirementsImpl","signatureNext":"checkRequirements"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
associatedtype c}
struct d<e: a, f: a>where e.c == f.c {
init(e, b: f}
func g<h>(i: h) {
let j = i struct k: a {
typealias c = h
l = d(j k())#^^#
