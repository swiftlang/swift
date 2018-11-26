// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O %s -o %t/StringMemoryTest
// RUN: %target-codesign %t/StringMemoryTest
// RUN: %target-run %t/StringMemoryTest | %FileCheck %s

// REQUIRES: optimized_stdlib
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

let str = "abcdefg\u{A758}hijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz\u{A759}"
let str2 = "abcdefg\u{A759}hijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz\u{A758}"

@inline(never)
func lookup(_ str: String, _ dict: [String: Int]) -> Bool {
  if let _ = dict[str] {
    return true
  }
  return false
}

@inline(never)
func uppercase(_ str: String) -> String {
      return str.uppercased()
}

@inline(never)
func lowercase(_ str: String) -> String {
      return str.lowercased()
}

/// Make sure the hash function does not leak.

let dict = [ "foo" : 1]
for _ in 0 ..< 10_000_000 {
  if lookup("\u{1F1E7}\u{1F1E7}", dict) {
    print("Found?!")
  }
  if uppercase(str) == "A" {
    print("Found?!")
  }
  if lowercase(str2) == "A" {
    print("Found?!")
  }
}

// CHECK-NOT: Found?!
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
