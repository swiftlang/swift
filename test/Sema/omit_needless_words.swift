// RUN: %target-parse-verify-swift -Womit-needless-words

class C1 {
  init(tasteString: String) { } // expected-warning{{'init(tasteString:)' could be named 'init(taste:)'}}{{8-8=taste }}
  func processWithString(string: String, forInt: Int) { } // expected-warning{{'processWithString(_:forInt:)' could be named 'processWith(_:for:)'}}{{8-25=processWith}}{{42-42=for }}
}

extension String {
  static var randomString: String { return "" } // expected-warning{{'randomString' could be named 'random'}}{{14-26=random}}
  var wonkycasedString: String { return self } // expected-warning{{'wonkycasedString' could be named 'wonkycased'}}{{7-23=wonkycased}}
}

func callSites(s: String) {
  let c1 = C1(tasteString: "blah") // expected-warning{{'init(tasteString:)' could be named 'init(taste:)'}}{{15-26=taste}}
  c1.processWithString("a", forInt: 1) // expected-warning{{'processWithString(_:forInt:)' could be named 'processWith(_:for:)'}}{{6-23=processWith}}{{29-35=for}}
  _ = String.randomString // expected-warning{{'randomString' could be named 'random'}}{{14-26=random}}
  _ = s.wonkycasedString // expected-warning{{'wonkycasedString' could be named 'wonkycased'}}{{9-25=wonkycased}}
}

struct MagicNameString { }

extension String {
  func appendStrings(strings: [String]) { } // expected-warning{{'appendStrings' could be named 'append'}}{{8-21=append}}
  func appendObjects(objects: [AnyObject]) { } // expected-warning{{'appendObjects' could be named 'append'}}{{8-21=append}}
  func appendMagicNameStrings(strings: [MagicNameString]) { } // expected-warning{{'appendMagicNameStrings' could be named 'append'}}{{8-30=append}}
}
