// RUN: %target-run-simple-swift

// REQUIRES: executable_test

import Swift
import StdlibUnittest

class Tag {}

struct Scalar {
  var str = ""
  var x = Tag()
  var style: BinaryChoice  = .zero
  enum BinaryChoice: UInt32 {
    case zero = 0
    case one
  }
}

public struct Sequence {
  var tag: Tag = Tag()
  var tag2: Tag = Tag()
}

enum Node {
  case scalar(Scalar)
  case sequence(Sequence)
}

func createOptionalNodeNil<T>(_ t: T) -> T? {
  return nil
}

func isNil<T>(_ t: T?) -> Bool {
  return t == nil
}

var tests = TestSuite("extra inhabitants shifts")


tests.test("test-shift-fix") {
  let opt = createOptionalNodeNil(Node.scalar(Scalar()))
  var res = false
  if isNil(opt) {
    res = true
  }
  expectEqual(true, res)
}

runAllTests()
