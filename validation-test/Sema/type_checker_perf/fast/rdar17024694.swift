// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -solver-disable-shrink -disable-constraint-solver-performance-hacks -solver-enable-operator-designated-types
// REQUIRES: tools-release,no_asserts

_ = (2...100).reversed().filter({ $0 % 11 == 0 }).map {
  "\($0) bottles of beer on the wall, \($0) bottles of beer;\n"
  + "  take eleven down, pass 'em around, \($0-11) bottles of beer on the wall!"
}
