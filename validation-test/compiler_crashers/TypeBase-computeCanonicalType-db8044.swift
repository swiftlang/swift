// {"kind":"typecheck","original":"08c866ec","signature":"swift::TypeBase::computeCanonicalType()","signatureAssert":"Assertion failed: (Result->isCanonical()), function computeCanonicalType","signatureNext":"removeShadowedDecls"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a where b == Self
  @resultBuilder struct c {
      buildBlock<each d: a>(_:
      >
    func buildBlock<each d: a>(repeat each d)
