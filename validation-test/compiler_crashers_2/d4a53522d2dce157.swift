// {"kind":"typecheck","original":"e0be092d","signature":"swift::constraints::IgnoreAssignmentDestinationType::diagnoseForAmbiguity(llvm::ArrayRef<std::__1::pair<swift::constraints::Solution const*, swift::constraints::ConstraintFix const*>>) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
stride = (a:2).0
