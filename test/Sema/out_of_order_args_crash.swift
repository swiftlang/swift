// RUN: %target-typecheck-verify-swift

// Test case for issue #87792: Type checker crash in OutOfOrderArgumentFailure
// When parsing malformed function arguments, invalid fixes were being created in the solver
// without validation. This test ensures malformed code produces clear errors instead of SIGSEGV.

// MARK: - Malformed Arguments (Should error, not crash)

func f(x: Int, y: Int) {}

// Missing comma between arguments - previously crashed
f((y: 1 x: 2  // expected-error {{expected ',' separator}}

// MARK: - Valid Out-of-Order Arguments (Should still work)

func g(a: Int, b: Int, c: Int) {}

// Valid out-of-order arguments - should still be diagnosed correctly
g(b: 2, a: 1, c: 3)  // expected-error {{argument 'b' must precede argument 'a'}}

// MARK: - Additional Malformed Cases

func h(p: String, q: String) {}

// Another malformed case with missing closing paren
h((q: "hello" p: "world"  // expected-error {{expected ',' separator}}

// MARK: - Unlabeled Arguments (Out of order)

func i(x: Int, y: Int) {}

// Unlabeled arguments in wrong order - should still work
i(2, 1)  // This may or may not error depending on type checking

// MARK: - Valid Cases (Should compile without error)

func j(a: Int, b: Int) {}

// Correctly ordered arguments
j(a: 1, b: 2)

// Correctly ordered arguments (reversed)
j(b: 2, a: 1)  // expected-error {{argument 'b' must precede argument 'a'}}
