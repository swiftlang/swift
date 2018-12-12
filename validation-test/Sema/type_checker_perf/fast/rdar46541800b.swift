// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -solver-enable-operator-designated-types

import SIMDOperators

func foo(args: [String] = ["A"]) {
  let _ = ["B"]
        + [String](repeating: "C", count: 1 * args.count)
        + [String](repeating: "D", count: 1 * args.count)
        + [String](repeating: "E", count: 1 * args.count)
        + [String](repeating: "F", count: 2 * args.count)
}
