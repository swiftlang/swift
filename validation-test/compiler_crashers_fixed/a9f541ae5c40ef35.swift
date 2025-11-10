// {"signature":"(anonymous namespace)::ABIDependencyEvaluator::computeABIDependenciesForModule(swift::ModuleDecl*)"}
// RUN: not %target-swift-frontend -typecheck %s
struct a { init? { init!
