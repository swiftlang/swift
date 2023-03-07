// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Onone -g -o %t/CrashAsync
// RUN: %target-codesign %t/CrashAsync
// RUN: (env SWIFT_BACKTRACE=enable=yes,cache=no %target-run %t/CrashAsync || true) | %FileCheck %s
// RUN: (env SWIFT_BACKTRACE=preset=friendly,enable=yes,cache=no %target-run %t/CrashAsync || true) | %FileCheck %s --check-prefix FRIENDLY

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx

@available(SwiftStdlib 5.1, *)
func crash() {
  let ptr = UnsafeMutablePointer<Int>(bitPattern: 4)!
  ptr.pointee = 42
}

@available(SwiftStdlib 5.1, *)
func level(_ n: Int) async {
  if n < 5 {
    await level(n + 1)
  } else {
    crash()
  }
}

@available(SwiftStdlib 5.1, *)
@main
struct CrashAsync {
  static func main() async {
    await level(1)
  }
}

// CHECK: *** Program crashed: Bad pointer dereference at 0x{{0+}}4 ***

// CHECK: Thread {{[0-9]+}} crashed:

// CHECK:  0                  0x{{[0-9a-f]+}} crash() + {{[0-9]+}} in CrashAsync at {{.*}}/CrashAsync.swift:16:15
// CHECK-NEXT:  1 [ra]             0x{{[0-9a-f]+}} level(_:) + {{[0-9]+}} in CrashAsync at {{.*}}/CrashAsync.swift:24:5
// CHECK-NEXT:  2 [async]          0x{{[0-9a-f]+}} level(_:) in CrashAsync at {{.*}}/CrashAsync.swift:22
// CHECK-NEXT:  3 [async]          0x{{[0-9a-f]+}} level(_:) in CrashAsync at {{.*}}/CrashAsync.swift:22
// CHECK-NEXT:  4 [async]          0x{{[0-9a-f]+}} level(_:) in CrashAsync at {{.*}}/CrashAsync.swift:22
// CHECK-NEXT:  5 [async]          0x{{[0-9a-f]+}} level(_:) in CrashAsync at {{.*}}/CrashAsync.swift:22
// CHECK-NEXT:  6 [async]          0x{{[0-9a-f]+}} static CrashAsync.main() in CrashAsync at {{.*}}/CrashAsync.swift:32
// CHECK-NEXT:  7 [async] [system] 0x{{[0-9a-f]+}} static CrashAsync.$main() in CrashAsync at {{.*}}/<compiler-generated>
// CHECK-NEXT:  8 [async] [system] 0x{{[0-9a-f]+}} async_MainTQ0_ in CrashAsync at {{.*}}/<compiler-generated>
// CHECK-NEXT:  9 [async] [thunk]  0x{{[0-9a-f]+}} thunk for @escaping @convention(thin) @async () -> () in CrashAsync at {{.*}}/<compiler-generated>
// CHECK-NEXT: 10 [async] [thunk]  0x{{[0-9a-f]+}} partial apply for thunk for @escaping @convention(thin) @async () -> () in CrashAsync at {{.*}}/<compiler-generated>
// CHECK-NEXT: 11 [async] [system] 0x{{[0-9a-f]+}} completeTaskWithClosure(swift::AsyncContext*, swift::SwiftError*) in libswift_Concurrency.dylib at {{.*}}/Task.cpp:463

// FRIENDLY: *** Program crashed: Bad pointer dereference at 0x{{0+}}4 ***

// FRIENDLY: Thread {{[0-9]+}} crashed:

// FRIENDLY: 0 crash() + {{[0-9]+}} in CrashAsync at {{.*}}CrashAsync.swift:16:15

// FRIENDLY:     14| func crash() {
// FRIENDLY-NEXT:     15|   let ptr = UnsafeMutablePointer<Int>(bitPattern: 4)!
// FRIENDLY-NEXT:  *  16|   ptr.pointee = 42
// FRIENDLY-NEXT:       |               ^
// FRIENDLY-NEXT:     17| }
// FRIENDLY-NEXT:     18|

// FRIENDLY: 1 level(_:) + {{[0-9]+}} in CrashAsync at {{.*}}CrashAsync.swift:24:5

// FRIENDLY:     22|     await level(n + 1)
// FRIENDLY-NEXT:     23|   } else {
// FRIENDLY-NEXT:  *  24|     crash()
// FRIENDLY-NEXT:       |     ^
// FRIENDLY-NEXT:     25|   }
// FRIENDLY-NEXT:     26| }

// FRIENDLY:2 level(_:) in CrashAsync at {{.*}}CrashAsync.swift:22

// FRIENDLY:     20| func level(_ n: Int) async {
// FRIENDLY-NEXT:     21|   if n < 5 {
// FRIENDLY-NEXT:  *  22|     await level(n + 1)
// FRIENDLY-NEXT:       |     ^
// FRIENDLY-NEXT:     23|   } else {
// FRIENDLY-NEXT:     24|     crash()

// FRIENDLY: 3 level(_:) in CrashAsync at {{.*}}CrashAsync.swift:22
// FRIENDLY: 4 level(_:) in CrashAsync at {{.*}}CrashAsync.swift:22
// FRIENDLY: 5 level(_:) in CrashAsync at {{.*}}CrashAsync.swift:22
// FRIENDLY: 6 static CrashAsync.main() in CrashAsync at {{.*}}CrashAsync.swift:32

// FRIENDLY:     30| struct CrashAsync {
// FRIENDLY-NEXT:     31|   static func main() async {
// FRIENDLY-NEXT:  *  32|     await level(1)
// FRIENDLY-NEXT:       |     ^
// FRIENDLY-NEXT:     33|   }
// FRIENDLY-NEXT:     34| }

