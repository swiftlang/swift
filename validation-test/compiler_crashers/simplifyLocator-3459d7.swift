// {"kind":"typecheck","original":"f153a9da","signature":"swift::constraints::simplifyLocator(swift::ASTNode&, llvm::ArrayRef<swift::constraints::ConstraintLocator::PathElement>&, swift::SourceRange&)","signatureNext":"ExpandArrayIntoVarargsFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
"\((+) as (String... , String) -> String)"
