// RUN: %target-typecheck-verify-swift

// Report the error but don't crash.

class TestUndefined {
  private var stringList: [String]!

  func dontCrash(strings: [String]) {
    assert(stringList.allSatisfy({ $0 == stringList.first!}))
    // expected-error@-1 {{use of local variable 'stringList' before its declaration}}
    let stringList = strings.filter({ $0 == "a" })
    // expected-note@-1 {{'stringList' declared here}}
  }
}
