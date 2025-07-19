// {"signature":"extendDepthMap(swift::Expr*, llvm::DenseMap<swift::Expr*, std::__1::pair<unsigned int, swift::Expr*>, llvm::DenseMapInfo<swift::Expr*, void>, llvm::detail::DenseMapPair<swift::Expr*, std::__1::pair<unsigned int, swift::Expr*>>>&)::RecordingTraversal::walkToExprPost(swift::Expr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  {
    func a {
      if
      #_hasSymbol(
