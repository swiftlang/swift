// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100

protocol SIMDVector: MutableCollection, Equatable where Index == Int {
  associatedtype Predicate: SIMDPredicate
  
  static func ==(lhs: Self, rhs: Self) -> Predicate
  static func !=(lhs: Self, rhs: Self) -> Predicate
}

protocol SIMDPredicate: SIMDVector where Element == Bool, Predicate == Self {
  static prefix func !(rhs: Self) -> Self
  static func &&(lhs: Self, rhs: Self) -> Self
  static func ||(lhs: Self, rhs: Self) -> Self
  static func all(_ predicate: Self) -> Bool
  static func any(_ predicate: Self) -> Bool
}

let samples: [UInt32] = []

func isCorrectHistogram(_ histogram: [(key: UInt32, value: Int)]) -> Bool {
    return histogram.count  == 157 &&
           histogram[0].0   == 0x00808080 && histogram[0].1   == 54 &&
           histogram[156].0 == 0x003B8D96 && histogram[156].1 == 1
}
