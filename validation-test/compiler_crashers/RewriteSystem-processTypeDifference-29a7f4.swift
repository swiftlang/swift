// {"kind":"typecheck","original":"0ffed59e","signature":"swift::rewriting::RewriteSystem::processTypeDifference(swift::rewriting::TypeDifference const&, unsigned int, unsigned int, swift::rewriting::RewritePath const&)","signatureAssert":"Assertion failed: (differenceID <= 0xffff), function getConcreteProjectionArg","signatureNext":"RewriteSystem::simplifyLeftHandSideSubstitutions"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  associatedtype c: a where c.b == (Self, b)
  associatedtype d: a where d.c == Self
}
