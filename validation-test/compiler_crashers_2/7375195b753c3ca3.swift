// {"signature":"fixRequirementFailure(swift::constraints::ConstraintSystem&, swift::Type, swift::Type, swift::ASTNode, llvm::ArrayRef<swift::constraints::ConstraintLocator::PathElement>)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a func b< each c : a >(repeat each c) b( d repeat {
