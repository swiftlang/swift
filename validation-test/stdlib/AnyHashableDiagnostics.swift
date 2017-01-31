// RUN: %target-typecheck-verify-swift

// If this test fails, the following types started to conditionally conform to
// `Hashable`.  When that happens, please add a custom `AnyHashable`
// representation to corresponding Objective-C types.

func isHashable<T : Hashable>(_: T.Type) {}

isHashable(Int.self) // no-error // Test that `isHashable(_:)` works.

isHashable(Array<Int>.self) // expected-error {{'Array<Int>' does not conform to expected type 'Hashable'}}
isHashable(Dictionary<Int, Int>.self) // expected-error {{'Dictionary<Int, Int>' does not conform to expected type 'Hashable'}}

