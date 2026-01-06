// RUN: %target-swift-emit-silgen -parse-as-library -target %target-swift-5.1-abi-triple %s | %FileCheck %s

// REQUIRES: concurrency

struct Err: Error { }

// CHECK: static func $main() async throws(Err)

// CHECK-LABEL: sil private [ossa] @async_Main : $@convention(thin) @async () -> () {
// CHECK: [[REPORT_FN:%.*]] = function_ref @$ss17_errorInMainTypedys5NeverOxs5ErrorRzlF
// CHECK-NEXT: apply [[REPORT_FN]]<Err>
@main
struct ThrowingMain {
  static func main() async throws(Err) { }
}
