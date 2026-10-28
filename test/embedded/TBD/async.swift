// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -validate-tbd-against-ir=all
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -validate-tbd-against-ir=all -O

// REQUIRES: swift_feature_Embedded
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1

import _Concurrency

public func asyncFunc() async -> Int { 1 }

public protocol AsyncRequirement {
  func requirement() async
}

public struct S: AsyncRequirement {
  public init() {}
  public func method() async {}
  public func requirement() async {}
}

public class C {
  public init() {}
  public func method() async {}
}

public actor A {
  public init() {}
  public func work() -> Int { 1 }
}
