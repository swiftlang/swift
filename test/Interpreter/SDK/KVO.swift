// RUN: %target-run-simple-swift-swift3 | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

var kvoContext = 0

class Model : NSObject {
  dynamic var name = ""
  dynamic var number = 0
}

class Observer : NSObject {
  let model = Model()

  override init() {
    super.init()
    model.addObserver(self, forKeyPath: "name", options: [], context: &kvoContext)
    self.addObserver(self, forKeyPath: "model.number", options: [], context: &kvoContext)
  }

  deinit {
    self.removeObserver(self, forKeyPath: "model.number")
    model.removeObserver(self, forKeyPath: "name")
  }

  func test() {
    model.name = "abc"
    model.number = 42
  }

  override func observeValue(forKeyPath keyPath: String?, of object: Any?, change: [NSKeyValueChangeKey : Any]?, context: UnsafeMutableRawPointer?) {
    if context != &kvoContext {
      // FIXME: we shouldn't need to unwrap these here, but it doesn't work on
      // older SDKs where these are non-optional types.
      return super.observeValue(forKeyPath: keyPath!, of: object!, change: change!, context: context)
    }

    print((object! as AnyObject).value(forKeyPath: keyPath!))
  }
}

// CHECK: abc
// CHECK-NEXT: 42
Observer().test()

class Foo: NSObject {
  let foo = 0
}

let foo = Foo()
foo.addObserver(foo, forKeyPath: "foo", options: [], context: &kvoContext)
let bar = foo.foo
// CHECK-NEXT: 0
print(bar)

let fooClass: AnyClass = object_getClass(foo)!
precondition(fooClass !== Foo.self, "no KVO subclass?")
precondition(fooClass is Foo.Type, "improper KVO subclass")
precondition(!(fooClass is Observer.Type), "improper KVO subclass")

let fooClassAsObject: AnyObject = fooClass
precondition(fooClassAsObject !== Foo.self, "no KVO subclass?")
precondition(fooClassAsObject is Foo.Type, "improper KVO subclass")
precondition(!(fooClassAsObject is Observer.Type), "improper KVO subclass")

let fooClassAsAny: Any = fooClass
precondition(fooClassAsAny is Foo.Type, "improper KVO subclass")
precondition(!(fooClassAsAny is Observer.Type), "improper KVO subclass")

// CHECK-NEXT: class metadata checks okay
print("class metadata checks okay")
