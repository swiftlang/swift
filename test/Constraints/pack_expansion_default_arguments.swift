// RUN: %target-typecheck-verify-swift

// Test that pack expansion types can be inferred from default argument values.
// This tests the fix for single-element tuple default arguments being flattened
// to scalars, which previously caused type inference to fail.

// Multi-element tuple default
func multiElementDefault<each T>(_ input: (repeat each T) = (1, "hello")) {
  print(input)
}

// Single-element default - the tuple (1) flattens to Int, but should still
// allow pack inference by wrapping the scalar in a single-element tuple
// before matching against the pack expansion tuple type.
func singleElementDefault<each T>(_ input: (repeat each T) = (1)) {
  print(input)
}

// Empty tuple default
func emptyDefault<each T>(_ input: (repeat each T) = ()) {
  print(input)
}

// With constraints
protocol P {}
extension Int: P {}
extension String: P {}

func constrainedMultiElementDefault<each T: P>(_ input: (repeat each T) = (1, 2)) {
  print(input)
}

func constrainedSingleElementDefault<each T: P>(_ input: (repeat each T) = (1)) {
  print(input)
}

// Test call sites
func testCallSites() {
  // Multi-element defaults
  multiElementDefault()
  multiElementDefault((1, 2, 3))

  // Single-element defaults
  singleElementDefault()
  singleElementDefault((42))
  singleElementDefault(42)  // equivalent to above

  // Empty defaults
  emptyDefault()
  emptyDefault(())

  // Constrained defaults
  constrainedMultiElementDefault()
  constrainedSingleElementDefault()
}
