// RUN: %target-typecheck-verify-swift -swift-version 4

//=-------------- SR-7295 --------------=/
class sr7295 {
  func doSomething(a: (() -> Void)? = nil, completion: @escaping ((String, Error?) -> Void)) {}
  func doSomething(b: @escaping ((String, Error?, Bool) -> Void)) {}
  func a() {
    doSomething(a: nil, completion: { _ in })
// expected-error@-1 {{contextual closure type '(String, Error?) -> Void' expects 2 arguments, but 1 was used in closure body}}
  }
}
