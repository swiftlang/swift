// {"kind":"typecheck","original":"e4c403e1","signature":"(anonymous namespace)::ExprRewriter::buildKeyPathSubscriptComponent(swift::constraints::SelectedOverload const&, swift::SourceLoc, swift::ArgumentList*, swift::constraints::ConstraintLocator*, llvm::SmallVectorImpl<swift::KeyPathExpr::Component>&)","signatureAssert":"Assertion failed: (argList->size() == indexHashables.size() || indexHashables.empty()), function Component","signatureNext":"ExprRewriter::visitKeyPathExpr"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<each b {
  subscript(repeat each b)  Int {
    c
    \a<Int, String>[2      ""]
