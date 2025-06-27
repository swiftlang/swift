// {"signature":"(anonymous namespace)::ABIDependencyEvaluator::computeABIDependenciesForModule(swift::ModuleDecl*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a let _ a.init = 0
