// {"signature":"swift::rewriting::RewriteSystem::decomposeTermIntoConformanceRuleLeftHandSides(swift::rewriting::MutableTerm, llvm::SmallVectorImpl<unsigned int>&) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a{associatedtype b : c} protocol c { associatedtype d }
struct e : a struct f < g : a, h : c where g.b.d == Int extension f where g == e
