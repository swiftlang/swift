// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/Shard
// RUN: %target-codesign %t/Shard

// Each shard prints a disjoint subset; the union must equal the full run.
// RUN: %target-run %t/Shard --stdlib-unittest-shard-count 2 --stdlib-unittest-shard-index 0 > %t/s0.txt
// RUN: %target-run %t/Shard --stdlib-unittest-shard-count 2 --stdlib-unittest-shard-index 1 > %t/s1.txt
// RUN: %target-run %t/Shard > %t/all.txt

// Shard 0 and shard 1 are disjoint. Their union is the full set of test cases.
// RUN: grep '^\[ *OK *\]' %t/s0.txt | sort > %t/s0.names
// RUN: grep '^\[ *OK *\]' %t/s1.txt | sort > %t/s1.names
// RUN: grep '^\[ *OK *\]' %t/all.txt | sort > %t/all.names
// RUN: cat %t/s0.names %t/s1.names | sort > %t/union.names
// RUN: diff %t/union.names %t/all.names
// RUN: comm -12 %t/s0.names %t/s1.names > %t/dup.names
// RUN: test ! -s %t/dup.names

// REQUIRES: executable_test

import StdlibUnittest

let ShardTests = TestSuite("Shard")
for i in 0..<20 {
  ShardTests.test("case\(i)") { expectEqual(i, i) }
}
runAllTests()
