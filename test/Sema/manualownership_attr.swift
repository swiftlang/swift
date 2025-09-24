// RUN: %target-typecheck-verify-swift \
// RUN:    -enable-experimental-feature NoImplicitCopy \
// RUN:    -enable-experimental-feature ManualOwnership

// REQUIRES: swift_feature_ManualOwnership
// REQUIRES: swift_feature_NoImplicitCopy

class C {}

@_manualOwnership
func hello() -> (C, C) {
  @_noImplicitCopy let x = C() // expected-error {{'@_noImplicitCopy' cannot be used with ManualOwnership}}
  return (x, x)
}
