// {"kind":"typecheck","original":"ffecb26f","signature":"swift::rewriting::RewriteSystem::minimizeRewriteSystem(swift::rewriting::PropertyMap const&)","signatureNext":"RequirementMachine::computeMinimalGenericSignature"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype Collapsed
}
protocol b : a {
  associatedtype c : b where c.Collapsed == Collapsed
  protocol d : b
    struct e
    : b {
      typealias Collapsed = e
      typealias c = e
      func f < g : d where g.c == e
