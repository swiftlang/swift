// {"signature":"diagnoseAmbiguityWithContextualType(swift::constraints::ConstraintSystem&, swift::constraints::SolutionDiff&, llvm::ArrayRef<std::__1::pair<swift::constraints::Solution const*, swift::constraints::ConstraintFix const*>>, llvm::ArrayRef<swift::constraints::Solution>)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a func b< c >(c = a(
    func a
