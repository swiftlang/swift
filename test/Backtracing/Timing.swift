// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Onone -g -o %t/Timing
// RUN: %target-codesign %t/Timing
// RUN: (env SWIFT_BACKTRACE=enable=yes,cache=no %target-run %t/Timing 2>&1 || true) | %FileCheck %s

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
struct Timing {
  static func main() {
    level1()
  }
}

// The backtracer should say how long the backtrace took, in seconds, with
// two decimal places.

// CHECK: Backtrace took {{[0-9]+}}.{{[0-9][0-9]}}s
