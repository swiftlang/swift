// RUN: %target-typecheck-verify-swift

protocol P {
  @_effects(readonly) func foo() // expected-note {{protocol requirement declared '@_effects(readonly)' here}}
}

struct MissingEffects: P {
  func foo() {} // expected-warning {{provides a weaker '@_effects' guarantee than its protocol requirement; add '@_effects(readonly)' to match}}
}

struct MatchingEffects: P {
  @_effects(readonly) func foo() {} // OK
}

struct StrongerEffects: P {
  @_effects(readnone) func foo() {} // OK: readnone is stronger than readonly
}
