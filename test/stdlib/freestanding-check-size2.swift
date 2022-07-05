// Check that we can LTO-optimize the freestanding/minimal stdlib against client usage of APIs, and that we don't produce unexpectedly large final binary.

// Important (!): This test is in test/stdlib/ to make sure it is actually run on the minimal/freestanding CI job, which
// filters the set of tests to test/stdlib/ only (see build-preset.ini).

// REQUIRES: freestanding
// REQUIRES: executable_test

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %{python} %utils/check_freestanding_size.py --path %t/a.out --triple %module-target-triple --size-path %llvm-size

let array = [1, 2, 3]
let dict = ["abc": 42]
let s = Set([1, 3, 5])
print("Hello \(array) \(dict) \(s)")
