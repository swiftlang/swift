// {"kind":"typecheck","original":"f5380d02","signature":"(anonymous namespace)::Classification::forDeclRef(swift::ConcreteDeclRef, (anonymous namespace)::ConditionalEffectKind, (anonymous namespace)::PotentialEffectReason, swift::SourceLoc, bool, std::__1::optional<swift::EffectKind>)","signatureAssert":"Assertion failed: (!thrownError->hasError()), function forThrows","signatureNext":"ApplyClassifier::classifyLookup"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  var b: Double {
    get throws(c) {
      b
    }
  }
}
