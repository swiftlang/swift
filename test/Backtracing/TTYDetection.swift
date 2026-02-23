// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Onone -g -o %t/TTYDetection
// RUN: %target-codesign %t/TTYDetection

// RUN: not --crash env SWIFT_BACKTRACE=enable=yes,cache=no %target-run %t/TTYDetection 2> %t/default-output && cat %t/default-output | %FileCheck %s
// RUN: not --crash env SWIFT_BACKTRACE=enable=yes,cache=no,output-to=stderr %target-run %t/TTYDetection 2> %t/stderr-output && cat %t/stderr-output | %FileCheck %s --check-prefix STDERR
// RUN: not --crash env SWIFT_BACKTRACE=enable=yes,cache=no,output-to=stdout %target-run %t/TTYDetection > %t/stdout-output && cat %t/stdout-output | %FileCheck %s --check-prefix STDOUT


// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: asan
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx || OS=linux-gnu
// COM: we should be able to add Windows to this test

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
struct TTYDetection {
  static func main() {
    level1()
  }
}

// CHECK: *** Program crashed: Bad pointer dereference at 0x{{0+}}4 ***

// CHECK: Thread 0 {{(".*" )?}}crashed:

// CHECK: 0               0x{{[0-9a-f]+}} level5() + {{[0-9]+}} in TTYDetection
// CHECK-NEXT: 1 [ra]          0x{{[0-9a-f]+}} level4() + {{[0-9]+}} in TTYDetection
// CHECK-NEXT: 2 [ra]          0x{{[0-9a-f]+}} level3() + {{[0-9]+}} in TTYDetection
// CHECK-NEXT: 3 [ra]          0x{{[0-9a-f]+}} level2() + {{[0-9]+}} in TTYDetection


// STDERR: *** Program crashed: Bad pointer dereference at 0x{{0+}}4 ***

// STDERR: Thread 0 {{(".*" )?}}crashed:

// STDERR: 0               0x{{[0-9a-f]+}} level5() + {{[0-9]+}} in TTYDetection
// STDERR-NEXT: 1 [ra]          0x{{[0-9a-f]+}} level4() + {{[0-9]+}} in TTYDetection
// STDERR-NEXT: 2 [ra]          0x{{[0-9a-f]+}} level3() + {{[0-9]+}} in TTYDetection
// STDERR-NEXT: 3 [ra]          0x{{[0-9a-f]+}} level2() + {{[0-9]+}} in TTYDetection

// STDOUT: *** Program crashed: Bad pointer dereference at 0x{{0+}}4 ***

// STDOUT: Thread 0 {{(".*" )?}}crashed:

// STDOUT: 0               0x{{[0-9a-f]+}} level5() + {{[0-9]+}} in TTYDetection
// STDOUT-NEXT: 1 [ra]          0x{{[0-9a-f]+}} level4() + {{[0-9]+}} in TTYDetection
// STDOUT-NEXT: 2 [ra]          0x{{[0-9a-f]+}} level3() + {{[0-9]+}} in TTYDetection
// STDOUT-NEXT: 3 [ra]          0x{{[0-9a-f]+}} level2() + {{[0-9]+}} in TTYDetection
