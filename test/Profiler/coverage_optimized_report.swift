// RUN: %empty-directory(%t)

// RUN: %target-build-swift %s -profile-generate -profile-coverage-mapping -O -o %t/main

// RUN: %target-codesign %t/main
// RUN: env %env-LLVM_PROFILE_FILE=%t/default.profraw %target-run %t/main

// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// RUN: %llvm-cov export -summary-only %t/main -instr-profile=%t/default.profdata | %FileCheck %s

// REQUIRES: profile_runtime
// REQUIRES: executable_test

// CHECK: "lines":{"count":9,"covered":5{{.*}}"functions":{"count":5,"covered":3

// The functions 'unused' and 'optimizedOut' will be optimized out, but
// make sure we still emit coverage records for them, using name data emitted
// separately in @__llvm_coverage_names.
func unused() {}
func optimizedOut() -> Int { .random() ? 1 : 2 }

func bar() -> Bool { false }

func baz() {
  if bar() {
    _ = optimizedOut()
  }
}

baz()
