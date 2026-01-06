// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library %import-static-libdispatch -Onone -static-stdlib -g -o %t/CrashStatic
// RUN: %target-codesign %t/CrashStatic
// RUN: (env SWIFT_BACKTRACE=enable=yes,cache=no,swift-backtrace=%backtracer %target-run %t/CrashStatic 2>&1 || true) | %FileCheck %s

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: asan
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: static_stdlib
// REQUIRES: OS=linux-gnu

func level1() {
  level2()
}

func level2() {
  level3()
}

func level3() {
  level4()
}

func level4() {
  level5()
}

func level5() {
  print("About to crash")
  let ptr = UnsafeMutablePointer<Int>(bitPattern: 4)!
  ptr.pointee = 42
}

@main
struct CrashStatic {
  static func main() {
    level1()
  }
}

// CHECK: *** Program crashed: Bad pointer dereference at 0x{{0+}}4 ***

// CHECK: Thread 0 {{(".*" )?}}crashed:

// CHECK: 0               0x{{[0-9a-f]+}} level5() + {{[0-9]+}} in CrashStatic at {{.*}}/CrashStatic.swift:33:15
// CHECK-NEXT: 1 [ra]          0x{{[0-9a-f]+}} level4() + {{[0-9]+}} in CrashStatic at {{.*}}/CrashStatic.swift:27:3
// CHECK-NEXT: 2 [ra]          0x{{[0-9a-f]+}} level3() + {{[0-9]+}} in CrashStatic at {{.*}}/CrashStatic.swift:23:3
// CHECK-NEXT: 3 [ra]          0x{{[0-9a-f]+}} level2() + {{[0-9]+}} in CrashStatic at {{.*}}/CrashStatic.swift:19:3
// CHECK-NEXT: 4 [ra]          0x{{[0-9a-f]+}} level1() + {{[0-9]+}} in CrashStatic at {{.*}}/CrashStatic.swift:15:3
// CHECK-NEXT: 5 [ra]          0x{{[0-9a-f]+}} static CrashStatic.main() + {{[0-9]+}} in CrashStatic at {{.*}}/CrashStatic.swift:39:5
// CHECK-NEXT: 6 [ra] [system] 0x{{[0-9a-f]+}} static CrashStatic.$main() + {{[0-9]+}} in CrashStatic at {{.*}}/<compiler-generated>
// CHECK-NEXT: 7 [ra] [system] 0x{{[0-9a-f]+}} main + {{[0-9]+}} in CrashStatic at {{.*}}/CrashStatic.swift

// CHECK: Registers:

// CHECK: Images ({{[0-9]+}} omitted):

// CHECK: {{0x[0-9a-f]+}}–{{0x[0-9a-f]+}}{{ +}}{{([0-9a-f]+|<no build ID>)}}{{ +}}CrashStatic{{ +}}{{.*}}/CrashStatic
