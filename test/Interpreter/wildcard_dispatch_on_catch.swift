// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

enum testError: Error {
  case C1
  case C2(Int, String)
}

public func someFunc(_ str: String) throws -> String {
  if (str[str.startIndex] == "D") {
    throw testError.C2(2, str)
  }
  return str
}

let testData: [String] = [
  "ABC",
  "DEF",
  ]

public func testCatchWildcardDispatch(_ name: String...) throws {
  for dir in testData {
    do {
      print(try someFunc(dir))
    } catch .C2(let errno, _) as testError where errno == 2 {
      print(name)
    }
  }
}

// CHECK: ABC
// CHECK: ["TEST"]
try testCatchWildcardDispatch("TEST")

