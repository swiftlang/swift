// {"signature":"swift::rewriting::RewriteSystem::verifyRewriteRules(swift::rewriting::RewriteSystem::ValidityPolicy) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a { associatedtype b : Collection }
struct c < d : Collection where d.Element : a,
    e == d.Element.b.Element struct f < d : Collection where d.Element : a,
    d.Element.b.Element == d.Index extension c where g == f<d>,
    d : RangeReplaceableCollection
