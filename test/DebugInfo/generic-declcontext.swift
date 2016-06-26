// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
public class C<T> {
  func getT() -> T? {
    let r : T? = nil
    return r
  }
}

public let t = C<Int>().getT()

public class D {
  public func foo() -> Double {
    let d = C<Double>().getT()
    return d!
  }
}

// CHECK: !DIGlobalVariable(name: "t", linkageName: "_Tv4main1tGSqSi_"
// CHECK-SAME:              type: ![[T:[0-9]+]]
// CHECK: ![[T]] = {{.*}}identifier: "_TtGSqSi_"

// CHECK: !DILocalVariable(name: "r", {{.*}}type: ![[R:[0-9]+]]
// CHECK: ![[R]] = {{.*}}identifier: "_TtGSqQq_C4main1C_"

// CHECK: !DILocalVariable(name: "d", {{.*}}type: ![[D:[0-9]+]]
// CHECK: ![[D]] = {{.*}}identifier: "_TtGSqSd_"

