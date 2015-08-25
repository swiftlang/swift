// RUN: %target-parse-verify-swift -Womit-needless-words

class C1 {
  init(string: String) { } // expected-warning{{'init(string:)' could be named 'init'}}{{8-8=_ }}
  func doSomethingWithString(string: String, andInt: Int) { } // expected-warning{{'doSomethingWithString(_:andInt:)' could be named 'doSomething(_:and:)'}}{{8-29=doSomething}}{{46-46=and }}
}

extension String {
  static var randomString: String { return "" } // expected-warning{{'randomString' could be named 'random'}}{{14-26=random}}
  var wonkycasedString: String { return self } // expected-warning{{'wonkycasedString' could be named 'wonkycased'}}{{7-23=wonkycased}}
}

func callSites(s: String) {
  let c1 = C1(string: "blah") // expected-warning{{'init(string:)' could be named 'init'}}{{15-23=}}
  c1.doSomethingWithString("a", andInt: 1) // expected-warning{{'doSomethingWithString(_:andInt:)' could be named 'doSomething(_:and:)'}}{{6-27=doSomething}}{{33-39=and}}
  _ = String.randomString // expected-warning{{'randomString' could be named 'random'}}{{14-26=random}}
  _ = s.wonkycasedString // expected-warning{{'wonkycasedString' could be named 'wonkycased'}}{{9-25=wonkycased}}
}
