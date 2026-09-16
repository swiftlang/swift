// -*- swift -*-

//===----------------------------------------------------------------------===//
// Automatically Generated From validation-test/stdlib/Slice/Inputs/Template.swift.gyb
// Do Not Edit Directly!
//===----------------------------------------------------------------------===//

// This file runs one shard of the tests defined in
// Inputs/Slice_Of_MinimalMutableRandomAccessCollection_FullWidth/main.swift,
// to limit each shard's running time to a reasonable amount.
// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s %S/Inputs/Slice_Of_MinimalMutableRandomAccessCollection_FullWidth/main.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out --stdlib-unittest-shard-count 4 --stdlib-unittest-shard-index 2

// REQUIRES: executable_test

// FIXME: the test is too slow when the standard library is not optimized.
// REQUIRES: optimized_stdlib
