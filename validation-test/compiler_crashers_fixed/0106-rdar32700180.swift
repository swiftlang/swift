// RUN: %target-swift-frontend %s -emit-ir
// REQUIRES: objc_interop

func f(_: AnyObject?) { }

class C {
  private var a: Int
  private var b: Int

  func test() {
    f((self.a, self.b) as AnyObject)
  }

  init() {
    a = 0
    b = 0
  }
}

