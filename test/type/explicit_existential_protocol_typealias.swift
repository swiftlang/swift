// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -enable-upcoming-feature ExistentialAny

// REQUIRES: swift_feature_ExistentialAny

protocol P {
  typealias PAlias1 = P

  func f1() -> any PAlias1
  func g1<T>(_: T) -> any PAlias1
}
extension P {
  typealias PAlias2 = P

  func f2() -> any PAlias2 {}
  func g2<T>(_: T) -> any PAlias2 {}
}
