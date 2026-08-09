// When an `expected-error` does not match the diagnostic that was produced, the
// verifier offers a fix-it rewriting the expectation to the actual message.
// llvm::SMDiagnostic::print lays fix-its out assuming one byte per column and
// asserts otherwise, so a message quoting a multi-byte character used to crash
// the verifier instead of reporting the mismatch.

// RUN: not %target-typecheck-verify-swift 2>&1 | %FileCheck %s

// The actual message quotes 'ꝸ', so no fix-it is offered and the expected text
// is spelled out in the error instead.
let ꝸ = 0
_ = ꝸꝸ
// expected-error@-1 {{deliberately wrong}}
// CHECK: error: incorrect message found; expected "cannot find 'ꝸꝸ' in scope"

// An all-ASCII message still gets the rewriting fix-it.
let s: Int = "not an int"
// expected-error@-1 {{also deliberately wrong}}
// CHECK: error: incorrect message found
// CHECK: cannot convert value of type 'String' to specified type 'Int'
