// RUN: %target-typecheck-verify-swift -enable-upcoming-feature InferSendableFromCaptures

// REQUIRES: objc_interop
// REQUIRES: concurrency
// REQUIRES: swift_feature_InferSendableFromCaptures

// This is a copy of test/expr/primary/keypath/keypath-observe-objc.swift with additional requirements to test sendable key paths

import Foundation

class Foo: NSObject {
  var number1 = 1
  dynamic var number2 = 2
  @objc var number3 = 3
  @objc dynamic var number4 = 4
  @objc var number5: Int {
    get { return 5 }
    set {}
  }
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

    _ = observe(\.foo.number5, options: [.new]) { _, change in // Okay
      print("observer4")
    }
  }
}

@_semantics("keypath.mustBeValidForKVO")
func quux<T, V, U>(_ object: T, at keyPath: KeyPath<T, V>, _ keyPath2: KeyPath<T, U>) { }

// The presence of a valid keypath should not prevent detection of invalid ones
// later in the argument list, so start with a valid one here.
quux(Foo(), at: \.number4, \.number1)
// expected-warning@-1 {{passing reference to non-'@objc dynamic' property 'number1' to KVO method 'quux(_:at:_:)' may lead to unexpected behavior or runtime trap}}
