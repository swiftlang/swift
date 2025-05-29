// {"signature":"(anonymous namespace)::ABIDependencyEvaluator::computeABIDependenciesForModule(swift::ModuleDecl*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
switch {
case init
