// RUN: %target-run-simple-swift
// REQUIRES: executable_test


// REQUIRES: objc_interop

import StdlibUnittest
import Foundation

protocol P { }
protocol Q { }

class Supermodel : NSObject { }

class Model1 : Supermodel {
  @objc dynamic var name = ""
}

class Model2 : NSObject, Q {
  @objc dynamic var name = ""
}


extension Supermodel: P { }

var kvoContext = 0

class Observer: NSObject {
  let model1 = Model1()
  let model2 = Model2()

  override init() {
    super.init()
    model1.addObserver(self, forKeyPath: "name", options: [], context: &kvoContext)
    model2.addObserver(self, forKeyPath: "name", options: [], context: &kvoContext)
  }

  deinit {
    model1.removeObserver(self, forKeyPath: "name")
    model2.removeObserver(self, forKeyPath: "name")
  }
}

let allTests = TestSuite("Dynamic subclasses conformance lookups")

allTests.test("Lookup via dynamic subclasses") {
  let observer = Observer()

  // Check via "AnyObject"
  let model1obj: AnyObject = observer.model1
  let model2obj: AnyObject = observer.model2

  expectTrue(model1obj is P)
  expectFalse(model1obj is Q)
  expectFalse(model2obj is P)
  expectTrue(model2obj is Q)

  expectNotNil(model1obj as? P)
  expectNil(model1obj as? Q)
  expectNil(model2obj as? P)
  expectNotNil(model2obj as? Q)
  
  // Check via "Any"
  let model1: Any = observer.model1
  let model2: Any = observer.model2

  expectTrue(model1 is P)
  expectFalse(model1 is Q)
  expectFalse(model2 is P)
  expectTrue(model2 is Q)

  expectNotNil(model1 as? P)
  expectNil(model1 as? Q)
  expectNil(model2 as? P)
  expectNotNil(model2 as? Q)
  
  print(model1)
  print(model2)
}

runAllTests()
