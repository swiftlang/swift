// RUN: not %target-swift-frontend -emit-ir %s -parse-stdlib -enable-experimental-feature Embedded 2>&1 | %FileCheck %s

public protocol Player {}
struct Concrete: Player {}

// CHECK: error: existential can cause metadata allocation or locks
public func test() -> any Player {
  Concrete()
}
