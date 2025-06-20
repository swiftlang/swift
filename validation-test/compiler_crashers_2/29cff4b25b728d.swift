// {"signature":"swift::TypeChecker::typeCheckStmtConditionElement(swift::StmtConditionElement&, bool&, swift::DeclContext*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a guard let b let a = b
