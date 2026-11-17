// {"extraArgs":["-enable-experimental-feature","FullTypedThrows"],"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::replaySolution(swift::constraints::Solution const&, bool)","signatureAssert":"Assertion failed: (sites.size() >= potentialThrowSites.size()), function replaySolution","signatureNext":"SplitterStep::mergePartialSolutions"}
// RUN: not --crash %target-swift-frontend -typecheck -enable-experimental-feature FullTypedThrows %s
// REQUIRES: swift_feature_FullTypedThrows
func a<b>()
do {
  a + 0
} catch <#expression#> {
}
