// RUN: %target-typecheck-verify-swift -enable-experimental-feature ASTGenTypes

// -enable-experimental-feature requires an asserts build
// REQUIRES: asserts


func test7(_ b: inout Bool) {
  b = true
}

struct X { struct `Protocol` { } }

func test10(_: X.`Protocol`) { }
