// {"kind":"typecheck","signature":"swift::LifetimeDependenceChecker::diagnoseMissingResultDependencies(swift::DiagID)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@abi(func a->b) func a < b
