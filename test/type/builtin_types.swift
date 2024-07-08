// RUN: %target-typecheck-verify-swift -parse-stdlib

import Swift

func testBuiltinModulePrint(_ builtin: Builtin.Int64) -> Bool {
  let x = 35
  return x == builtin // expected-error {{binary operator '==' cannot be applied to operands of type 'Int' and 'Builtin.Int64'}}
  // expected-note@-1 {{overloads for '==' exist with these partially matching parameter lists: (Int, Int)}}
}
