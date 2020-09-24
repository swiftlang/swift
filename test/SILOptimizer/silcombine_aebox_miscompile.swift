// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O -module-name=a %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

// This is an end-to-end test for rdar://problem/50759056.

enum ErrorEnum: Error {
  case errorCase([Error])
  case other
}

final class Myclass {
  var e = [Error]()
  var b = true

  @inline(never)
  func foo() {
    e.append(ErrorEnum.other)
    if b {
      bar(ErrorEnum.errorCase(e))
    }
  }

  @inline(never)
  func bar(_: Error?) {
    b = false
    foo()
  }
}

let c = Myclass()
c.foo()

// CHECK: [a.ErrorEnum.other, a.ErrorEnum.other]
print(c.e)
