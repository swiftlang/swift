// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

public protocol P {
  var v : Int32 { get };
}

public extension P {
  // CHECK: define {{.*}}swiftcc i32 @"$s4main1PPAAE1fs5Int32VyF"
  public func f() -> Int32 {
    // CHECK-NEXT: entry:
    // CHECK-NEXT: %[[ALLOCA:.*]] = alloca ptr,
    // CHECK-NEXT: #dbg_declare(ptr %[[ALLOCA]],
    // CHECK-SAME:    ![[SELFMETA:.*]], !DIExpression()
    return v
  }
}

// CHECK-DAG: ![[SELFMETA]] = !DILocalVariable(name: "$\CF\84_0_0", {{.*}}type: ![[SELFTY:[0-9]+]], flags: DIFlagArtificial)
// CHECK-DAG: ![[SELFTY]] = !DIDerivedType(tag: DW_TAG_typedef, name: "Self"
