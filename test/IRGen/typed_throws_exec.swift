// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name=test %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

public enum MyError : Error {
  case a
  case b
}

public func throwing() throws(MyError) -> Int {
  throw MyError.a
}

func wat() {
  fatalError("this cannot happen")
}

public func catching() {
  do {
    try throwing()
    wat()
  } catch {
  }
}

catching()
print("Works!")
// CHECK: Works!
