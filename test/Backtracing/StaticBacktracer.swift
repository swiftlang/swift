// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Onone -g -o %t/StaticBacktracer
// RUN: %target-codesign %t/StaticBacktracer
// RUN: /usr/bin/ldd %backtracer-static | %FileCheck %s --check-prefix LIBS
// RUN: (env SWIFT_BACKTRACE=enable=yes,cache=no,swift-backtrace=%backtracer-static %target-run %t/StaticBacktracer 2>&1 || true) | %FileCheck %s

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
struct StaticBacktracer {
  static func main() {
    level1()
  }
}

// CHECK: *** Program crashed: Bad pointer dereference at 0x{{0+}}4 ***

// CHECK: Thread 0 {{(".*" )?}}crashed:

// CHECK: 0               0x{{[0-9a-f]+}} level5() + {{[0-9]+}} in StaticBacktracer at {{.*}}/StaticBacktracer.swift:34:15
// CHECK-NEXT: 1 [ra]          0x{{[0-9a-f]+}} level4() + {{[0-9]+}} in StaticBacktracer at {{.*}}/StaticBacktracer.swift:28:3
// CHECK-NEXT: 2 [ra]          0x{{[0-9a-f]+}} level3() + {{[0-9]+}} in StaticBacktracer at {{.*}}/StaticBacktracer.swift:24:3
// CHECK-NEXT: 3 [ra]          0x{{[0-9a-f]+}} level2() + {{[0-9]+}} in StaticBacktracer at {{.*}}/StaticBacktracer.swift:20:3
// CHECK-NEXT: 4 [ra]          0x{{[0-9a-f]+}} level1() + {{[0-9]+}} in StaticBacktracer at {{.*}}/StaticBacktracer.swift:16:3
// CHECK-NEXT: 5 [ra]          0x{{[0-9a-f]+}} static StaticBacktracer.main() + {{[0-9]+}} in StaticBacktracer at {{.*}}/StaticBacktracer.swift:40:5
// CHECK-NEXT: 6 [ra] [system] 0x{{[0-9a-f]+}} static StaticBacktracer.$main() + {{[0-9]+}} in StaticBacktracer at {{.*}}/<compiler-generated>
// CHECK-NEXT: 7 [ra] [system] 0x{{[0-9a-f]+}} main + {{[0-9]+}} in StaticBacktracer at {{.*}}/StaticBacktracer.swift

// CHECK: Registers:

// CHECK: Images ({{[0-9]+}} omitted):

// CHECK: {{0x[0-9a-f]+}}–{{0x[0-9a-f]+}}{{ +}}{{([0-9a-f]+|<no build ID>)}}{{ +}}StaticBacktracer{{ +}}{{.*}}/StaticBacktracer

// .............................................................................

// We mustn't have any Swift libraries dynamically linked here; that's the
// entire point.

// LIBS-NOT: libswift{{.*}}.so
