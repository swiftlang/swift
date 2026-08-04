// RUN: %target-swift-frontend -emit-ir -parse-as-library %s | %FileCheck %s

// REQUIRES: concurrency

// Each of the partial functions that the implementation produces for an 'async'
// function respects the '@section' attribute.

@inline(never)
func other(_ x: Int) async -> Int { x + 1 }

@section("__TEXT,boot")
public func bootAsync() async -> Int {
  let first = await other(1)
  return await other(first)
}

// CHECK: define{{.*}} @"$s13section_async9bootAsyncSiyYaF"({{.*}}section "__TEXT,boot"
// CHECK: define{{.*}} @"$s13section_async9bootAsyncSiyYaFTQ0_"({{.*}}section "__TEXT,boot"
// CHECK: define{{.*}} @"$s13section_async9bootAsyncSiyYaFTQ1_"({{.*}}section "__TEXT,boot"
