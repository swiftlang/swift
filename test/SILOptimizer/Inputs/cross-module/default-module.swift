
import Submodule

public func incrementByThree(_ x: Int) -> Int {
  return incrementByOne(x) + 2
}

public func incrementByThreeWithCall(_ x: Int) -> Int {
  return incrementByOneNoCMO(x) + 2
}
