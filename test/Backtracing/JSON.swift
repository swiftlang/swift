// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Onone -g -o %t/JSON
// RUN: %target-codesign %t/JSON
// RUN: (env SWIFT_BACKTRACE=enable=yes,cache=no %target-run %t/JSON 2>&1 || true) | %FileCheck %s

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: asan
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx || OS=linux-gnu

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
struct JSON {
  static func main() {
    level1()
  }
}

// CHECK: *** Program crashed: Bad pointer dereference at 0x{{0+}}4 ***

// CHECK: Thread 0 {{(".*" )?}}crashed:

// CHECK: 0               0x{{[0-9a-f]+}} level5() + {{[0-9]+}} in JSON at {{.*}}/JSON.swift:42:15
// CHECK-NEXT: 1 [ra]          0x{{[0-9a-f]+}} level4() + {{[0-9]+}} in JSON at {{.*}}/JSON.swift:36:3
// CHECK-NEXT: 2 [ra]          0x{{[0-9a-f]+}} level3() + {{[0-9]+}} in JSON at {{.*}}/JSON.swift:32:3
// CHECK-NEXT: 3 [ra]          0x{{[0-9a-f]+}} level2() + {{[0-9]+}} in JSON at {{.*}}/JSON.swift:28:3
// CHECK-NEXT: 4 [ra]          0x{{[0-9a-f]+}} level1() + {{[0-9]+}} in JSON at {{.*}}/JSON.swift:24:3
// CHECK-NEXT: 5 [ra]          0x{{[0-9a-f]+}} static JSON.main() + {{[0-9]+}} in JSON at {{.*}}/JSON.swift:48:5
// CHECK-NEXT: 6 [ra] [system] 0x{{[0-9a-f]+}} static JSON.$main() + {{[0-9]+}} in JSON at {{.*}}/<compiler-generated>
// CHECK-NEXT: 7 [ra] [system] 0x{{[0-9a-f]+}} main + {{[0-9]+}} in JSON at {{.*}}/JSON.swift

// CHECK: Registers:

// CHECK: Images ({{[0-9]+}} omitted):

// CHECK: {{0x[0-9a-f]+}}–{{0x[0-9a-f]+}}{{ +}}{{([0-9a-f]+|<no build ID>)}}{{ +}}JSON{{ +}}{{.*}}/JSON
