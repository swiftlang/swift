// RUN: %target-typecheck-verify-swift -swift-version 4

// https://github.com/apple/swift/issues/49843
class C_49843 {
  func doSomething(a: (() -> Void)? = nil, completion: @escaping ((String, Error?) -> Void)) {}
  func doSomething(b: @escaping ((String, Error?, Bool) -> Void)) {}
  func a() {
    doSomething(a: nil, completion: { _ in })
// expected-error@-1 {{contextual closure type '(String, (any Error)?) -> Void' expects 2 arguments, but 1 was used in closure body}}
  }
}
