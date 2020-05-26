// RUN: %target-typecheck-verify-swift -parse-stdlib

import Swift

func testBuiltinModulePrint(_ builtin: Builtin.Int64) -> Bool {
  let x = 35
  return x == builtin // expected-error {{operator function '==' requires that 'Builtin.Int64' conform to 'BinaryInteger'}}
}
