// {"signature":"(anonymous namespace)::ABIDependencyEvaluator::computeABIDependenciesForModule(swift::ModuleDecl*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a { init? { init!
