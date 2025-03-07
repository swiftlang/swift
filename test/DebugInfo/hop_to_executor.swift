// RUN: %target-swiftc_driver %s -c -g -Onone -o - -Xllvm -sil-print-debuginfo -emit-sil -parse-as-library -module-name m |  %FileCheck %s

// This test ensures that the hop_to_executor source location matches the end of the do block

func getTimestamp(x: Int) async -> Int {
  return 40 + x
}
func work() {}
func foo() async {
  do {
    work()
    async let timestamp2 = getTimestamp(x:2)
    print(await timestamp2)
  // CHECK: %[[REG:[0-9]+]] = function_ref @swift_asyncLet_finish : $@convention(thin) @async (Builtin.RawPointer, Builtin.RawPointer) -> (), loc {{.*}}:[[@LINE+3]]
  // CHECK-NEXT: %{{[0-9]+}} = apply %[[REG]](%{{[0-9]+}}, %{{[0-9]+}}) : $@convention(thin) @async (Builtin.RawPointer, Builtin.RawPointer) -> (), loc{{.*}}:[[@LINE+2]]
  // CHECK-NEXT: hop_to_executor %0, loc * {{.*}}:[[@LINE+1]]
  }
  work()
}
@main enum entry {
  static func main() async {
    await foo()
  }
}
