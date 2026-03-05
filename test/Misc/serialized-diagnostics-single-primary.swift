// Test that in single-primary-per-job mode, diagnostics from a non-primary
// file are NOT written into the primary file's .dia output — even when the
// primary's type-checking triggers type-checking of the non-primary's body
// (i.e. the realistic cross-file-reference scenario).
//
// We simulate two separate swift-frontend jobs, each with one primary file:
//
//  Job A: primary = this file (no errors, but uses HelperWrapper from helper),
//         non-primary = helper (has error in HelperWrapper's body).
//         After fix: helper's error is triggered during Job A's type-checking
//         but is eaten → job succeeds, .dia has no errors from helper.
//
//  Job B: primary = helper (has error), non-primary = this file.
//         helper's error goes into helper's .dia.
//
// RUN: rm -f %t.*

// ---------- Job A: this file is primary, helper is non-primary ----------
// The primary uses HelperWrapper, which forces the compiler to type-check
// the helper's body and triggers the helper's error.  After the fix the error
// is eaten, so compilation succeeds (no `not` prefix).
// RUN: %target-swift-frontend -typecheck \
// RUN:   -primary-file %s \
// RUN:   -serialize-diagnostics-path %t.main.dia \
// RUN:   %S/Inputs/serialized-diagnostics-single-primary-helper.swift \
// RUN:   2> %t.jobA.stderr.txt

// RUN: c-index-test -read-diagnostics %t.main.dia 2> %t.main.txt

// The helper's error must NOT appear in Job A's diagnostics.
// NEGATIVE-MAIN-NOT: serialized-diagnostics-single-primary-helper.swift

// ---------- Job B: helper is primary, this file is non-primary ----------
// The helper has a type error so compilation fails.
// RUN: not %target-swift-frontend -typecheck \
// RUN:   -primary-file %S/Inputs/serialized-diagnostics-single-primary-helper.swift \
// RUN:   -serialize-diagnostics-path %t.helper.dia \
// RUN:   %s \
// RUN:   2> %t.jobB.stderr.txt

// RUN: c-index-test -read-diagnostics %t.helper.dia 2> %t.helper.txt

// The helper's error MUST appear in Job B's diagnostics.
// CHECK-HELPER: serialized-diagnostics-single-primary-helper.swift:{{[0-9]+}}:{{[0-9]+}}: error:

// RUN: %FileCheck -check-prefix=NEGATIVE-MAIN %s < %t.main.txt
// RUN: %FileCheck -check-prefix=CHECK-HELPER %s < %t.helper.txt
// RUN: %FileCheck --allow-empty -check-prefix=NEGATIVE-STDERR %s < %t.jobA.stderr.txt
// NEGATIVE-STDERR-NOT: serialized-diagnostics-single-primary-helper.swift

// Use HelperWrapper so the compiler must type-check the helper's body.
func usesHelper(_ h: HelperWrapper) {}
