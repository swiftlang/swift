// REQUIRES: profile_runtime
// REQUIRES: executable_test

// This test is to make sure llvm-cov can deal with a coverage-prefix-map.

// To make sure this test is resilient to directory changes, we create nested
// directories inside of the temporary test directory.
//
// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/root/nested
// RUN: echo "func coverage() {}" > %t/root/nested/coverage_relative_path.swift
// RUN: cd %t/root

// RUN: %target-build-swift -profile-generate -profile-coverage-mapping -Xfrontend -coverage-prefix-map -Xfrontend %t/root=. -Xfrontend -disable-incremental-llvm-codegen -o %t/main %t/root/nested/coverage_relative_path.swift

// RUN: %target-codesign %t/main
// RUN: env %env-LLVM_PROFILE_FILE=%t/default.profraw %target-run %t/main

// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// RUN: %llvm-cov show %t/main -instr-profile=%t/default.profdata | %FileCheck --check-prefix SHOW %s
// RUN: %llvm-cov report %t/main -instr-profile=%t/default.profdata | %FileCheck --check-prefix REPORT %s

// SHOW: func coverage
// REPORT: coverage_relative_path.swift
