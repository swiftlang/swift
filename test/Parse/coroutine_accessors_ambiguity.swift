// RUN: %target-typecheck-verify-swift \
// RUN:     -verify-additional-prefix enabled- \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -debug-diagnostic-names
// RUN: %target-typecheck-verify-swift \
// RUN:     -verify-additional-prefix disabled- \
// RUN:     -debug-diagnostic-names

// REQUIRES: swift_feature_CoroutineAccessors

// Properties with implicit getters which call functions named modify.

func modify<T>(_ c : () -> T) -> T { c() }
func read<T>(_ c : () -> T) -> T { c() }

// enabled: need reader
// disabled: ok!
var im : Int {
  yielding mutate { // expected-error{{variable with a 'yielding mutate' accessor must also have a getter, addressor, or 'yielding borrow' accessor}}
    1 // expected-warning{{integer literal is unused}}
  }
}

// enabled: ok
// disabled: ok!
var ir : Int {
  yielding borrow {
    1 // expected-warning{{integer literal is unused}}
  }
}
