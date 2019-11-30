// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

import Foundation

class Foo: NSObject {
  var number1 = 1
  dynamic var number2 = 2
  @objc var number3 = 3
  @objc dynamic var number4 = 4
}

class Bar: NSObject {
  @objc dynamic let foo: Foo
  
  init(foo: Foo) {
    self.foo = foo
    super.init()
    
    _ = observe(\.foo.number1, options: [.new]) { _, change in
      // expected-warning@-1 {{passing reference to non-'@objc dynamic' property 'number1' to KVO method 'observe(_:options:changeHandler:)' may lead to unexpected behavior or runtime trap}}
      print("observer1")
    }
    
    _ = observe(\.foo.number2, options: [.new]) { _, change in
      // expected-warning@-1 {{passing reference to non-'@objc dynamic' property 'number2' to KVO method 'observe(_:options:changeHandler:)' may lead to unexpected behavior or runtime trap}}
      print("observer2")
    }
    
    _ = observe(\.foo.number3, options: [.new]) { _, change in
      // expected-warning@-1 {{passing reference to non-'@objc dynamic' property 'number3' to KVO method 'observe(_:options:changeHandler:)' may lead to unexpected behavior or runtime trap}}
      print("observer3")
    }
    
    _ = observe(\.foo.number4, options: [.new]) { _, change in // Okay
      print("observer4")
    }
  }
}
