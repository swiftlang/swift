// RUN: %target-swift-frontend -emit-silgen %s -module-name a -swift-version 5  -target %target-swift-5.1-abi-triple -Xllvm -sil-print-debuginfo -emit-verbose-sil -parse-as-library | %FileCheck %s
// REQUIRES: concurrency

// Test that the async dispatch code in the prologue has auto-generated debug
// locations.

@MainActor func f() async -> Int {
// CHECK: {{^sil .*}}@$s1a1fSiyYaF
// CHECK: %0 = metatype      {{.*}}loc * "{{.*}}.swift":[[@LINE-2]]:17,{{.*}}:auto_gen
// CHECK: %1 = function_ref  {{.*}}loc * "{{.*}}.swift":[[@LINE-3]]:17,{{.*}}:auto_gen
// CHECK: %2 = apply %1(%0)  {{.*}}loc * "{{.*}}.swift":[[@LINE-4]]:17,{{.*}}:auto_gen
// CHECK: begin_borrow       {{.*}}loc * "{{.*}}.swift":[[@LINE-5]]:17,{{.*}}:auto_gen
// CHECK: hop_to_executor    {{.*}}loc * "{{.*}}.swift":[[@LINE-6]]:17,{{.*}}
// CHECK: // end sil function '$s1a1fSiyYaF'
  return 23
}
@main struct Main {
  static func main() async {
    let x = await f();
  }
}
