// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

2...100.reversed().filter({ $0 % 11 == 0 }).map {
  // expected-error@-1 {{value of type 'Int' has no member 'reversed'}}
  "\($0) bottles of beer on the wall, \($0) bottles of beer;\n"
  + "  take eleven down, pass 'em around, \($0-11) bottles of beer on the wall!"
}
