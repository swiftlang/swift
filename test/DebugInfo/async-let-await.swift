// RUN: %target-swift-frontend %s -emit-ir -g -o - \
// RUN:    -module-name M  -target %target-swift-5.1-abi-triple \
// RUN:    -parse-as-library | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize

// REQUIRES: concurrency
// REQUIRES: CPU=x86_64 || CPU=arm64

public func getVegetables() async -> [String] {
  return ["leek", "carrot"]  
}

// CHECK: define {{.*}} @"$s1M14chopVegetablesSaySSGyYaKFTQ0_"
public func chopVegetables() async throws -> [String] {
  let veggies = await getVegetables()
  // CHECK-NOT: {{^define }}
  // CHECK:  #dbg_declare(ptr %0, ![[V:[0-9]+]], !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_deref, DW_OP_plus_uconst, {{[0-9]+}})
  // CHECK: ![[V]] = !DILocalVariable(name: "veggies"
  return veggies.map { "chopped \($0)" }
}
