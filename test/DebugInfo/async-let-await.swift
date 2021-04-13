// RUN: %target-swift-frontend %s -emit-ir -g -o - \
// RUN:    -module-name M -enable-experimental-concurrency \
// RUN:    -parse-as-library | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize
// REQUIRES: concurrency
// UNSUPPORTED: CPU=arm64e

public func getVegetables() async -> [String] {
  return ["leek", "carrot"]  
}

// CHECK: define {{.*}} @"$s1M14chopVegetablesSaySSGyYaKFTQ0_"
public func chopVegetables() async throws -> [String] {
  let veggies = await getVegetables()
  // CHECK-NOT: {{^define }}
  // CHECK:  call void @llvm.dbg.declare(metadata i8* %0, metadata ![[V:[0-9]+]], metadata !DIExpression(DW_OP_deref
  // CHECK: ![[V]] = !DILocalVariable(name: "veggies"
  return veggies.map { "chopped \($0)" }
}
