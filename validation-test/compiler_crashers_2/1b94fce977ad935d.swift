// {"signature":"swift::DifferentiableAttr::DifferentiableAttr(bool, swift::SourceLoc, swift::SourceRange, swift::DifferentiabilityKind, llvm::ArrayRef<swift::ParsedAutoDiffParameter>, swift::TrailingWhereClause*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(@differentiable _ =
