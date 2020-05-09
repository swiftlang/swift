// RUN: %target-typecheck-verify-swift

class C {}
func overload(x: Int) -> C {}
func overload(x: Float) -> C? {}

// Ensure that we infer both weak vars as having a single level of
// optionality. We will emit diagnostics if that is not the case.
weak var v = overload(x: 1)
weak var w = overload(x: 1.0)
