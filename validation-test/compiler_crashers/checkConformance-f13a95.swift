// {"kind":"typecheck","original":"8e2479db","signature":"swift::checkConformance(swift::Type, swift::ProtocolDecl*, bool)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"must take a contextual type. if you really are ok with an \" \"indefinite answer (and usually YOU ARE NOT), then consider whether \" \"you really, definitely are ok with an indefinite answer, and \" \"use `checkConformanceWithoutContext` instead\"), function checkConformance"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a<b, c> {
  associatedtype b
  associatedtype c
}
struct d<e, f>: a where e: a {
  typealias b = e.b
  typealias c = f
  let first: e
}
func g<h>() {
  struct i: a {
    typealias b = h
    func j<k>() -> a<h, k> {
      d(self)
    }
  }
}
