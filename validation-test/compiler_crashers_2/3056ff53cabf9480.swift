// {"signature":"swift::constraints::ConstraintSystem::setClosureType(swift::ClosureExpr const*, swift::FunctionType*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol b let c = {(a : b)in switch a{case.d {
