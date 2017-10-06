// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O %s -o %t/StringMemoryTest
// RUN: %target-run %t/StringMemoryTest | %FileCheck %s

// REQUIRES: optimized_stdlib
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

@inline(never)
func lookup(_ str: String, _ dict: [String: Int]) -> Bool {
  if let _ = dict[str] {
    return true
  }
  return false
}

/// Make sure the hash function does not leak.

let dict = [ "foo" : 1]
for _ in 0 ..< 10_000_000 {
  if lookup("\u{1F1E7}\u{1F1E7}", dict) {
    print("Found?!")
  }
}

// CHECK: Not found
print("Not found")

var usage = rusage()
getrusage(RUSAGE_SELF, &usage)

// CHECK: success
// CHECK-NOT: failure

// We should not need 50MB for this.
if usage.ru_maxrss > 50 * 1024 * 1024 {
  print("failure - should not need 50MB!")
} else {
  print("success")
}
