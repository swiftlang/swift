// RUN: %target-swift-frontend -O %s -emit-sil -o /dev/null

public class X {}

public func testit(_ s: [X]) -> X? {
  var r: X?
  for e in s {
    if r == nil {
      r = e
    } else if e !== r {
      return nil
    }
  }
  return r
}

