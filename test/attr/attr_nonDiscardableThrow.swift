// RUN: %target-typecheck-verify-swift

// @_nonDiscardableThrow requires @discardableResult

@_nonDiscardableThrow // expected-error {{'@_nonDiscardableThrow' must be combined with '@discardableResult'}}
func missingDiscardableResult(_ op: () throws -> Void) -> Int { return 0 }

// @_nonDiscardableThrow requires a closure parameter that uses typed throws

@discardableResult
@_nonDiscardableThrow // expected-error {{'@_nonDiscardableThrow' requires the function to have a closure parameter that uses typed throws}}
func noClosureParam() -> Int { return 0 }

@discardableResult
@_nonDiscardableThrow // expected-error {{'@_nonDiscardableThrow' requires the function to have a closure parameter that uses typed throws}}
func nonThrowingClosure(_ op: () -> Void) -> Int { return 0 }

// Valid: has @discardableResult and a throwing closure parameter

@discardableResult
@_nonDiscardableThrow
func validThrowingClosure(_ op: () throws -> Void) -> Int { return 0 }

@discardableResult
@_nonDiscardableThrow
func validTypedThrowingClosure<E: Error>(_ op: () throws(E) -> Void) -> Int { return 0 }

// Test the behavior: throwing closure warns, non-throwing closure does not

struct MyError: Error {}

func testNonDiscardableThrow() {
  validThrowingClosure { throw MyError() }
  // expected-warning @-1 {{is not used, which may accidentally ignore errors thrown inside the task}}
  // expected-note @-2 {{to silence this warning, handle the error inside the task, or store/discard the task value explicitly}}
  validThrowingClosure { } // no warning - closure does not throw
  _ = validThrowingClosure { throw MyError() } // no warning - explicitly discarded
}
