// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -num-threads 0 -emit-ir %S/Inputs/coverage_num_threads1.swift | %FileCheck %s -check-prefix=SINGLE-SOURCE --implicit-check-not="llvm_coverage_mapping =" --implicit-check-not="@__covrec_{{[a-zA-Z0-9]+}} ="

// Make sure we only emit 1 coverage record in total.
// SINGLE-SOURCE: @__covrec_{{[a-zA-Z0-9]+}} =
// SINGLE-SOURCE: llvm_coverage_mapping =

// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -num-threads 0 -emit-ir %S/Inputs/coverage_num_threads1.swift %S/Inputs/coverage_num_threads2.swift | %FileCheck %s -check-prefix=SINGLE-OBJECT --implicit-check-not="llvm_coverage_mapping =" --implicit-check-not="@__covrec_{{[a-zA-Z0-9]+}} ="

// Make sure we only emit 2 coverage records in total.
// SINGLE-OBJECT: @__covrec_{{[a-zA-Z0-9]+}} =
// SINGLE-OBJECT: @__covrec_{{[a-zA-Z0-9]+}} =
// SINGLE-OBJECT: llvm_coverage_mapping =

// Using 1 goes down the multithreaded codepath but only operates with a single thread to work around an issue on Windows where the output of both IR modules is interleaved and therefore the output is invalid
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -num-threads 1 -emit-ir %S/Inputs/coverage_num_threads1.swift %S/Inputs/coverage_num_threads2.swift | %FileCheck %s -check-prefix=MULTIPLE-OBJECTS --implicit-check-not="llvm_coverage_mapping =" --implicit-check-not="@__covrec_{{[a-zA-Z0-9]+}} ="

// Make sure we only emit 2 coverage records in total (1 per output).
// MULTIPLE-OBJECTS: @__covrec_{{[a-zA-Z0-9]+}} =
// MULTIPLE-OBJECTS: llvm_coverage_mapping =
// MULTIPLE-OBJECTS: ; ModuleID =
// MULTIPLE-OBJECTS: @__covrec_{{[a-zA-Z0-9]+}} =
// MULTIPLE-OBJECTS: llvm_coverage_mapping =

// Under -O, we inline the profiler increment of func2 into func1. The coverage
// mapping for func2 should still however be present only in its original file.
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -num-threads 1 -O -emit-sil %S/Inputs/coverage_num_threads3.swift %S/Inputs/coverage_num_threads4.swift | %FileCheck %s -check-prefix=MULTIPLE-OBJECTS-INLINE-SIL
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -num-threads 1 -O -emit-ir %S/Inputs/coverage_num_threads3.swift %S/Inputs/coverage_num_threads4.swift | %FileCheck %s -check-prefix=MULTIPLE-OBJECTS-INLINE --implicit-check-not="llvm_coverage_mapping =" --implicit-check-not="@__covrec_{{[a-zA-Z0-9]+}} ="

// MULTIPLE-OBJECTS-INLINE-SIL-LABEL: sil @$s21coverage_num_threads35func1yyF
// MULTIPLE-OBJECTS-INLINE-SIL:       increment_profiler_counter 0, "$s21coverage_num_threads35func1yyF"
// MULTIPLE-OBJECTS-INLINE-SIL:       increment_profiler_counter 0, "$s21coverage_num_threads35func2yyF"

// MULTIPLE-OBJECTS-INLINE-SIL-LABEL: sil @$s21coverage_num_threads35func2yyF
// MULTIPLE-OBJECTS-INLINE-SIL:       increment_profiler_counter 0, "$s21coverage_num_threads35func2yyF"

// MULTIPLE-OBJECTS-INLINE: @__covrec_{{[a-zA-Z0-9]+}} =
// MULTIPLE-OBJECTS-INLINE: llvm_coverage_mapping =
// MULTIPLE-OBJECTS-INLINE: ; ModuleID =
// MULTIPLE-OBJECTS-INLINE: @__covrec_{{[a-zA-Z0-9]+}} =
// MULTIPLE-OBJECTS-INLINE: llvm_coverage_mapping =
