// NOTE: This intentionally doesn't use %target-typecheck-verify, this should
// compile without any errors since we're testing the ASTVerifier is happy.
// RUN: %target-swift-frontend -typecheck %s

// Make sure we don't prematurely mark 'x' as invalid.
func testInferenceFromClosureVar<T>(x: T = { var x: Int = 0; return x }()) {}
