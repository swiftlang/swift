//===----------------------------------------------------------------------===//
// Automatically Generated From validation-test/stdlib/Array/Inputs/ArrayConformanceTests.swift.gyb
// Do Not Edit Directly!
//===----------------------------------------------------------------------===//

// This file runs one shard of the tests defined in
// Inputs/ArraySliceWithNonZeroStartIndex_MutableRandomAccessCollectionVal/main.swift,
// to limit each shard's running time to a reasonable amount.
// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s %S/Inputs/ArraySliceWithNonZeroStartIndex_MutableRandomAccessCollectionVal/main.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %enable-cow-checking %target-run %t/a.out --stdlib-unittest-shard-count 4 --stdlib-unittest-shard-index 2

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
