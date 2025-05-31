// {"signature":"(anonymous namespace)::ABIDependencyEvaluator::computeABIDependenciesForModule(swift::ModuleDecl*)"}
// RUN: not %target-swift-frontend -typecheck %s
switch {
case init
