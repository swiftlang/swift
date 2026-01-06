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
  // CHECK:     %{{[0-9]+}} = builtin "finishAsyncLet"(%{{[0-9]+}}, %{{[0-9]+}}) : $(), loc{{.*}}:[[@LINE+2]]
  // CHECK-NEXT: hop_to_executor %0, loc * {{.*}}:[[@LINE+1]]
  }
  work()
}
@main enum entry {
  static func main() async {
    await foo()
  }
}
