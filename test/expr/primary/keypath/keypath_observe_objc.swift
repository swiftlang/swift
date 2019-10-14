// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library  %s -verify

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import Foundation
import ObjectiveC

class Foo: NSObject {
  var number1 = 1
  dynamic var number2 = 2
  @objc var number3 = 3
  @objc dynamic var number4 = 4
}

class Bar: NSObject {
  @objc dynamic let foo: Foo
  var observer1: NSKeyValueObservation?
  var observer2: NSKeyValueObservation?
  var observer3: NSKeyValueObservation?
  var observer4: NSKeyValueObservation?
  
  init(foo: Foo) {
    self.foo = foo
    super.init()
    
    observer1 = observe(\.foo.number1, options: [.new]) { _, change in
      // expected-warning@-1 {{passing reference to non-'@objc dynamic' property 'number1' to KVO method 'observe(_:options:changeHandler:)' may lead to unexpected behavior or runtime trap}}
      print("observer1")
    }
    
    observer2 = observe(\.foo.number2, options: [.new]) { _, change in
      // expected-warning@-1 {{passing reference to non-'@objc dynamic' property 'number2' to KVO method 'observe(_:options:changeHandler:)' may lead to unexpected behavior or runtime trap}}
      print("observer2")
    }
    
    observer3 = observe(\.foo.number3, options: [.new]) { _, change in
      // expected-warning@-1 {{passing reference to non-'@objc dynamic' property 'number3' to KVO method 'observe(_:options:changeHandler:)' may lead to unexpected behavior or runtime trap}}
      print("observer3")
    }
    
    observer4 = observe(\.foo.number4, options: [.new]) { _, change in // Okay
      print("observer4")
    }
  }
}
