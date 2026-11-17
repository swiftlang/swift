// RUN: %target-run-simple-swift(-O -enable-experimental-feature BuiltinModule -enable-experimental-feature Lifetimes) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_Lifetimes

// Check that alias analysis considers that a function with a scoped lifetime dependence
// on an inout argument can escape the address of the argument into its result.

import Builtin

extension MutableSpan {
  @_lifetime(&value)
  @inline(never)
  init(single value: inout Element) {
    let address = Builtin.addressof(&value)
    let span = unsafe MutableSpan(_unsafeStart: .init(address), count: 1)
    self = unsafe _overrideLifetime(span, mutating: &value)
  }
}

@inline(never)
func updateLocalViaSpan() -> Int {
  var value = 42
  var span = MutableSpan(single: &value)
  span[0] += 1
  _ = consume span
  return value
}

// CHECK: 43
print(updateLocalViaSpan())
