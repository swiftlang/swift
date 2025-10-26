// RUN: %target-swift-frontend %s -sil-verify-all -c

// Make sure we don't crash.

class TestUndefined {
  private var stringList: [String]!

  func dontCrash(strings: [String]) {
    assert(stringList.allSatisfy({ $0 == stringList.first!}))
    let stringList = strings.filter({ $0 == "a" })
  }
}
