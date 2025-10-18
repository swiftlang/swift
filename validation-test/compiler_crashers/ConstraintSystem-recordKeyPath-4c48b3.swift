// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::recordKeyPath(swift::KeyPathExpr const*, swift::TypeVariableType*, swift::TypeVariableType*, swift::DeclContext*)","signatureAssert":"Assertion failed: (inserted), function recordKeyPath"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a { b->a{switch self{case.c(\ d) e
