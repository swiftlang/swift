// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

// CHECK-NOT: sil_global private [let] {{.*}}unused1{{.*}}
private let unused1 = 0
// CHECK-NOT: sil_global private {{.*}}unused2{{.*}}
private var unused2 = 42
// CHECK: sil_global private [let] @${{.*}}used1{{.*}} : $Int
private let used1 = 0
// CHECK: sil_global private @${{.*}}used2{{.*}} : $Int
private var used2 = 0

// CHECK: sil_global [let] @${{.*}}unused3{{.*}} : $Int
public let unused3 = 0
// CHECK: sil_global @${{.*}}unused4{{.*}} : $Int
public var unused4 = 0

// These should only be optimized with -wmo.
// CHECK: sil_global hidden [let] @${{.*}}unused5{{.*}} : $Int
let unused5 = 0
// CHECK: sil_global hidden @${{.*}}unused6{{.*}} : $Int
var unused6 = 0

// CHECK-LABEL: sil [Onone] @${{.*}}test{{.*}}
@_optimize(none) public func test(x: Int) -> Int {
  // CHECK: %{{[0-9]+}} = global_addr @${{.*}}used2{{.*}}
  // CHECK: %{{[0-9]+}} = global_addr @${{.*}}used1{{.*}}
  return used1 + used2 + x
}
