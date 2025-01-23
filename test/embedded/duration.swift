// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -wmo
// RUN: %target-swift-emit-ir -O %s -enable-experimental-feature Embedded -wmo
// RUN: %target-swift-emit-ir -Osize %s -enable-experimental-feature Embedded -wmo

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

@available(SwiftStdlib 5.7, *)
extension Duration {
  @available(SwiftStdlib 5.7, *)
  public init() {
    self = .seconds(10) + .nanoseconds(20)
  }
}
