// {"signature":"swift::constraints::ConstraintSystem::recordKeyPath(swift::KeyPathExpr const*, swift::TypeVariableType*, swift::TypeVariableType*, swift::DeclContext*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a { b->a{switch self{case.c(\ d) e
