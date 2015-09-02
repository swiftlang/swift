// RUN: %target-parse-verify-swift -Womit-needless-words

class C1 {
  init(titleString: String) { } // expected-warning{{'init(titleString:)' could be named 'init(title:)'}}{{8-8=title }}
  func doSomethingWithString(string: String, andInt: Int) { } // expected-warning{{'doSomethingWithString(_:andInt:)' could be named 'doSomethingWith(_:and:)'}}{{8-29=doSomethingWith}}{{46-46=and }}
}

extension String {
  static var randomString: String { return "" } // expected-warning{{'randomString' could be named 'random'}}{{14-26=random}}
  var wonkycasedString: String { return self } // expected-warning{{'wonkycasedString' could be named 'wonkycased'}}{{7-23=wonkycased}}
}

func callSites(s: String) {
  let c1 = C1(titleString: "blah") // expected-warning{{'init(titleString:)' could be named 'init(title:)'}}{{15-26=title}}
  c1.doSomethingWithString("a", andInt: 1) // expected-warning{{'doSomethingWithString(_:andInt:)' could be named 'doSomethingWith(_:and:)'}}{{6-27=doSomethingWith}}{{33-39=and}}
  _ = String.randomString // expected-warning{{'randomString' could be named 'random'}}{{14-26=random}}
  _ = s.wonkycasedString // expected-warning{{'wonkycasedString' could be named 'wonkycased'}}{{9-25=wonkycased}}
}
