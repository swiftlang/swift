; Verify that an error-severity LLVM backend diagnostic is turned into a
; frontend failure instead of being silently swallowed while an empty/partial
; object file is left behind (which previously surfaced only as a confusing
; downstream "file is empty" link error).
;
; The backend error is triggered deterministically and target-independently by
; an invalid instruction in module-level inline asm, which MC object emission
; reports as an error-severity diagnostic through the LLVMContext handler.

; This error is specific per backend, so we just check AArch64.

; REQUIRES: CODEGENERATOR=AArch64

; RUN: %empty-directory(%t)

; By default the backend diagnostic message is swallowed, but the recorded
; error now makes IRGen fail and emit its own diagnostic instead of reporting
; success.
; RUN: not %target-swift-frontend -target arm64-apple-ios7.0 -emit-object -o %t/out.o %s 2>&1 | %FileCheck %s

; CHECK: error: could not emit object file '{{.*}}out.o': the LLVM backend reported an error; please submit a bug report
; CHECK-NOT: {{invalid|unrecognized}} instruction mnemonic

; With -print-llvm-backend-diagnostics, LLVM's underlying error message is
; printed so the root cause is visible.
; RUN: not %target-swift-frontend -target arm64-apple-ios7.0 -emit-object -print-llvm-backend-diagnostics -o %t/out2.o %s 2>&1 | %FileCheck -check-prefix=CHECK-VERBOSE %s

; CHECK-VERBOSE: {{invalid|unrecognized}} instruction mnemonic

module asm "this_is_not_a_valid_instruction"
