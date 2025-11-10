// {"signature":"(anonymous namespace)::ApplyClassifier::classifyApply(swift::ApplyExpr*, llvm::DenseSet<swift::Expr const*, llvm::DenseMapInfo<swift::Expr const*, void>>*)"}
// RUN: not %target-swift-frontend -typecheck %s
let a = switch  \0 {
case 0.0
