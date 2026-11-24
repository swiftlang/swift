// {"kind":"typecheck","original":"49190d01","signature":"swift::rewriting::RewriteSystem::computeTypeDifference(swift::rewriting::Term, swift::rewriting::Symbol, swift::rewriting::Symbol, std::__1::optional<unsigned int>&, std::__1::optional<unsigned int>&)","signatureNext":"PropertyMap::unifyConcreteTypes"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  protocol c<b>: a {
    func g<
      d,
      e: c<(b...) -> d>where e.b == (f) -> d
    >()
  }
}
