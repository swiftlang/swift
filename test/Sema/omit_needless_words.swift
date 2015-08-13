// RUN: %target-parse-verify-swift -Womit-needless-words

class C1 {
  init(string: String) { } // expected-warning{{'init(string:)' could be named 'init'}}{{8-8=_ }}
  func doSomethingWithString(string: String, andInt: Int) { } // expected-warning{{'doSomethingWithString(_:andInt:)' could be named 'doSomething(_:and:)'}}{{8-29=doSomething}}{{46-46=and }}
}

extension String {
  var randomString: String { return self } // expected-warning{{'randomString' could be named 'random'}}{{7-19=random}}
}
