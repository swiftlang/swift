// RUN: not %target-swift-frontend %s -sil-verify-all -c 2>&1 | %FileCheck %s

// Report the error but don't crash.
// CHECK: error: closure captures 'stringList' before it is declared

class TestUndefined {
  private var stringList: [String]!

  func dontCrash(strings: [String]) {
    assert(stringList.allSatisfy({ $0 == stringList.first!}))
    let stringList = strings.filter({ $0 == "a" })
  }
}
