// RUN: %target-typecheck-verify-swift

protocol ScalarOrArray {
}

protocol SpecialValue: ScalarOrArray {
}

extension Array: ScalarOrArray where Element: SpecialValue {
}

struct CustomArray : ScalarOrArray {
}

extension CustomArray : ExpressibleByArrayLiteral {
  public init(arrayLiteral elements: Int32...) {
    self.init()
  }
}

extension Int32: SpecialValue {
}

func +<T: ScalarOrArray>(_: T, _: CustomArray) -> CustomArray {}
func +<T: ScalarOrArray>(_: CustomArray, _: T) -> CustomArray {}
func +(_: CustomArray, _: CustomArray) -> CustomArray {}

extension Sequence where Element == Int {
  var asInt32: [Int32] { [] }
}

extension Array where Element == Int {
  var asInt32: [Int32] { [] }
}

func test(v: Int32, b: [Int]) -> [Int32] {
  let result = [1, v - v] + b.asInt32
  return result // Ok
}
