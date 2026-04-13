// {"kind":"typecheck","original":"67628027","signature":"swift::checkConformance(swift::Type, swift::ProtocolDecl*, bool)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"must take a contextual type. if you really are ok with an \" \"indefinite answer (and usually YOU ARE NOT), then consider whether \" \"you really, definitely are ok with an indefinite answer, and \" \"use `checkConformanceWithoutContext` instead\"), function checkConformance","signatureNext":"canSynthesize"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b> {
  class c {
    class d: c, Codable {
    }
  }
}
