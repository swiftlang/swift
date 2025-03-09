// RUN: %target-swiftc_driver %s -c -g -Onone -o - -Xllvm -sil-print-debuginfo -emit-sil -parse-as-library -module-name m |  %FileCheck %s

// This test ensures that the hop_to_executor source location matches the end of the do block

// CHECK: %35 = function_ref @swift_asyncLet_finish : $@convention(thin) @async (Builtin.RawPointer, Builtin.RawPointer) -> (), loc {{.*}}:19:3, scope 8
// CHECK-NEXT: %36 = apply %35(%10, %5) : $@convention(thin) @async (Builtin.RawPointer, Builtin.RawPointer) -> (), loc{{.*}}:19:3
// CHECK-NEXT: hop_to_executor %0, loc * {{.*}}:19:3, scope 8 


func getTimestamp(x: Int) async -> Int {
  return 40 + x
}
func work() {}
func foo() async {
  do {
    work()
    async let timestamp2 = getTimestamp(x:2)
    print(await timestamp2)
  }
  work()
}
@main enum entry {
  static func main() async {
    await foo()
  }
}
