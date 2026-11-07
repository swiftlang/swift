// {"kind":"typecheck","original":"e4c403e1","signature":"(anonymous namespace)::ExprRewriter::buildKeyPathSubscriptComponent(swift::constraints::SelectedOverload const&, swift::SourceLoc, swift::ArgumentList*, swift::constraints::ConstraintLocator*, llvm::SmallVectorImpl<swift::KeyPathExpr::Component>&)","signatureAssert":"Assertion failed: (argList->size() == indexHashables.size() || indexHashables.empty()), function Component","signatureNext":"ExprRewriter::visitKeyPathExpr"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<each b {
  subscript(repeat each b)  Int {
    c
    \a<Int, String>[2      ""]
