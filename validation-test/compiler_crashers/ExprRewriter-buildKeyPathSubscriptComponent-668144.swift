// {"kind":"typecheck","original":"34cf40cd","signature":"(anonymous namespace)::ExprRewriter::buildKeyPathSubscriptComponent(swift::constraints::SelectedOverload const&, swift::SourceLoc, swift::ArgumentList*, swift::constraints::ConstraintLocator*, llvm::SmallVectorImpl<swift::KeyPathExpr::Component>&)","signatureAssert":"Assertion failed: (hashableConformance), function buildKeyPathSubscriptComponent","signatureNext":"ExprRewriter::visitKeyPathExpr"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<each b {
  subscript(repeat each b)  Int
  func c<each d: Hashable>(e: repeat each d) {
    \a<repeat each d>[repeat each e]
