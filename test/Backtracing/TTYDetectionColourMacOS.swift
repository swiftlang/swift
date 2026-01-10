// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Onone -g -o %t/TTYDetection
// RUN: %target-codesign %t/TTYDetection

// RUN: (env SWIFT_BACKTRACE=enable=yes,cache=no,interactive=no %target-run script -reF %t/script-output %t/TTYDetection || true) && cat %t/script-output | %FileCheck %s


// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: asan
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx
// COM: this won't work on Windows, but is not critical function

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

// CHECK: 💣{{.*}}Program crashed: Bad pointer dereference at 0x{{0+}}4
// CHECK: Thread 0 {{(".*" )?}}crashed:
