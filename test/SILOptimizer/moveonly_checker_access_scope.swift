// RUN: %target-swift-frontend  %s -emit-sil -o /dev/null

// Check that the compiler doesn't crash.

struct S {
  var i: Int64

  consuming func testit() -> S {
    self.i = Int64.init(Double.init(self.i))
    return self
  }
}
