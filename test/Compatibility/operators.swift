// RUN: %target-typecheck-verify-swift -swift-version 4

// expect-no-diagnostics

struct X { }

postfix func ++(x: X) -> X { return x }
prefix func ++(x: X) -> X { return x }
postfix func --(x: X) -> X { return x }
prefix func --(x: X) -> X { return x }
