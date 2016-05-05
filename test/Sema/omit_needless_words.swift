// RUN: %target-parse-verify-swift -Womit-needless-words

class C1 {
  init(tasteString: String) { } // expected-warning{{'init(tasteString:)' could be named 'init(taste:)'}}{{8-8=taste }}
  func processWithString(_ string: String, toInt: Int) { } // expected-warning{{'processWithString(_:toInt:)' could be named 'process(with:to:)'}}{{8-25=process}} {{26-27=with}} {{44-44=to }}
  func processWithInt(_ value: Int) { } // expected-warning{{'processWithInt' could be named 'process(with:)'}}{{8-22=process}}
}

extension String {
  static var randomString: String { return "" } // expected-warning{{'randomString' could be named 'random'}}{{14-26=random}}
  var wonkycasedString: String { return self } // expected-warning{{'wonkycasedString' could be named 'wonkycased'}}{{7-23=wonkycased}}
}

func callSites(_ s: String) {
  let c1 = C1(tasteString: "blah") // expected-warning{{'init(tasteString:)' could be named 'init(taste:)'}}{{15-26=taste}}
  c1.processWithString("a", toInt: 1) // expected-warning{{'processWithString(_:toInt:)' could be named 'process(with:to:)'}}{{6-23=process}}{{29-34=to}}
  c1.processWithInt(5) // expected-warning{{'processWithInt' could be named 'process(with:)'}}{{6-20=process}}
  _ = String.randomString // expected-warning{{'randomString' could be named 'random'}}{{14-26=random}}
  _ = s.wonkycasedString // expected-warning{{'wonkycasedString' could be named 'wonkycased'}}{{9-25=wonkycased}}
}

struct MagicNameString { }

extension String {
  func appendStrings(_ strings: [String]) { } // expected-warning{{'appendStrings' could be named 'append'}}{{8-21=append}}
  func appendObjects(_ objects: [AnyObject]) { } // expected-warning{{'appendObjects' could be named 'append'}}{{8-21=append}}
  func appendMagicNameStrings(_ strings: [MagicNameString]) { } // expected-warning{{'appendMagicNameStrings' could be named 'append'}}{{8-30=append}}
}

class NSArray {
  func arrayByAddingObject(_ x: AnyObject) -> NSArray { return NSArray() } // expected-warning{{'arrayByAddingObject' could be named 'adding' [-Womit-needless-words]}}{{8-27=adding}}
}

func emptyFirstParamName(_: Int) { }
