// {"kind":"typecheck","signature":"(anonymous namespace)::Classification::forDeclRef(swift::ConcreteDeclRef, (anonymous namespace)::ConditionalEffectKind, (anonymous namespace)::PotentialEffectReason, swift::SourceLoc, bool, std::__1::optional<swift::EffectKind>)","signatureAssert":"Assertion failed: (!thrownError->hasError()), function forThrows"}
// RUN: not --crash %target-swift-frontend -typecheck %s
var a : String { get throws(b{
  a
