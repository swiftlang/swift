// {"kind":"typecheck","original":"0729f5a5","signature":"swift::rewriting::RewriteSystem::processTypeDifference(swift::rewriting::TypeDifference const&, unsigned int, unsigned int, swift::rewriting::RewritePath const&)","signatureAssert":"Assertion failed: (differenceID <= 0xffff), function getConcreteProjectionArg","signatureNext":"PropertyMap::unifyConcreteTypes"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b : a
  struct c<d, e : a> : a {
    typealias b = c<d, e.b>
    func f < h, g where h == c<h, g>, g.b == h.b>()
  }
}
