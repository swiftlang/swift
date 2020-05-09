// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation

var ObjCFailableInitTestSuite = TestSuite("ObjCFailableInit")

class Canary {
  static var count: Int = 0

  init() {
    Canary.count += 1
  }

  deinit {
    Canary.count -= 1
  }
}

extension NSDate {
  @objc convenience init?(b: Bool) {
    guard b else { return nil }
    self.init()
  }
}

class MyDate : NSDate {
  var derivedCanary = Canary()

  static var count = 0

  override init() {
    MyDate.count += 1
    super.init()
  }

  required convenience init(coder: NSCoder) {
    fatalError("not implemented")
  }

  deinit {
    MyDate.count -= 1
  }

  @objc convenience init?(b: Bool) {
    guard b else { return nil }
    self.init()
  }
}

class MyDerivedDate : MyDate {
  var canary = Canary()

  static var derivedCount = 0

  override init() {
    MyDerivedDate.count += 1
  }

  deinit {
    MyDerivedDate.count -= 1
  }

}

func mustFail<T>(f: () -> T?) {
  if f() != nil {
    preconditionFailure("Didn't fail")
  }
}

ObjCFailableInitTestSuite.test("InitFailure_Before") {
  mustFail { NSDate(b: false) }
  expectEqual(0, Canary.count)

  mustFail { MyDate(b: false) }
  expectEqual(0, Canary.count)
  expectEqual(0, MyDate.count)

  mustFail { MyDerivedDate(b: false) }
  expectEqual(0, Canary.count)
  expectEqual(0, MyDate.count)
  expectEqual(0, MyDerivedDate.derivedCount)
}

runAllTests()

