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
func getMemoryUsage() -> Int {
  var usage = rusage()
  getrusage(RUSAGE_SELF, &usage)
  return usage.ru_maxrss
}

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

@inline(never)
func runTest() {
  for _ in 0 ..< 15_0000 {
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
}


/// Make sure the hash function does not leak.

let dict = [ "foo" : 1]

let baseUsage = getMemoryUsage()
runTest()
let firstRun = getMemoryUsage () - baseUsage
runTest()
runTest()
runTest()
let secondRun = getMemoryUsage () - baseUsage

// CHECK-NOT: Found?!
// CHECK: Not found

print("Not found")

// CHECK: success
// CHECK-NOT: failure

if firstRun * 3 < secondRun && firstRun > 10_000 {
  print("failure - should not linearly increase firstRun: \(firstRun) secondRun: \(secondRun)")
} else {
  print("success firstRun: \(firstRun) secondRun: \(secondRun)")
}
