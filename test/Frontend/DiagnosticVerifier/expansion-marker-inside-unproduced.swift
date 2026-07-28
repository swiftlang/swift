// A '// #name@N' marker defined inside an expansion block lives inside that
// block regardless of whether the expansion is actually produced, so it
// shouldn't be reported as misplaced.

// RUN: not %target-typecheck-verify-swift 2>&1 | %FileCheck %s --implicit-check-not "only allowed inside"

// CHECK: :[[@LINE+1]]:4: error: expected expansion not produced
// expected-expansion@+3:14{{
//   expected-error@1{{oops}}
//   #inBlock@2
// }}
func inBlockUnproduced() {}
