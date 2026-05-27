// {"kind":"typecheck","original":"b8fc10e1","signature":"(anonymous namespace)::Classification::forDeclRef(swift::ConcreteDeclRef, (anonymous namespace)::ConditionalEffectKind, (anonymous namespace)::PotentialEffectReason, swift::SourceLoc, bool, std::__1::optional<swift::EffectKind>)","signatureAssert":"Assertion failed: (!thrownError->hasError()), function forThrows","signatureNext":"ApplyClassifier::classifyFunctionBody"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a: AsyncIteratorProtocol {
  mutating <#declaration#>
  b { next(
  }
