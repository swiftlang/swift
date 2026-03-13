// {"kind":"typecheck","signature":"checkRequirementsImpl(llvm::ArrayRef<swift::Requirement>, bool)","signatureAssert":"Assertion failed: ((allowTypeParameters || !firstType->hasTypeParameter()) && \"must take a contextual type. if you really are ok with an \" \"indefinite answer (and usually YOU ARE NOT), then consider whether \" \"you really, definitely are ok with an indefinite answer, and \" \"use `checkRequirementsWithoutContext` instead\"), function checkRequirementsImpl"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < Element {
  b {
    class c : Collection { typealias Index = d subscript(e :
