// RUN: %target-run-simple-swift | FileCheck %s

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
    model.addObserver(self, forKeyPath: "name", options: nil, context: &kvoContext)
    self.addObserver(self, forKeyPath: "model.number", options: nil, context: &kvoContext)
  }

  deinit {
    self.removeObserver(self, forKeyPath: "model.number")
    model.removeObserver(self, forKeyPath: "name")
  }

  func test() {
    model.name = "abc"
    model.number = 42
  }

  override func observeValueForKeyPath(keyPath: String?, ofObject object: AnyObject?, change: [NSObject : AnyObject]?, context: UnsafeMutablePointer<Void>) {
    if context != &kvoContext {
      // FIXME: we shouldn't need to unwrap these here, but it doesn't work on
      // older SDKs where these are non-optional types.
      return super.observeValueForKeyPath(keyPath!, ofObject: object!, change: change!, context: context)
    }

    print(object!.valueForKeyPath(keyPath!))
  }
}

// CHECK: abc
// CHECK-NEXT: 42
Observer().test()

class Foo: NSObject {
  let foo = 0
}

let foo = Foo()
foo.addObserver(foo, forKeyPath: "foo", options: nil, context: &kvoContext)
let bar = foo.foo
// CHECK-NEXT: 0
print(bar)
