// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Xfrontend -parse-as-library -Xfrontend -disable-availability-checking -Onone -o %t/SimpleBacktrace.exe
// RUN: %target-codesign %t/SimpleBacktrace.exe
// RUN: %target-run %t/SimpleBacktrace.exe | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx || OS=linux-gnu || OS=windows-msvc

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Runtime

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
  let backtrace = try! Backtrace.capture()

  // CHECK:      0{{[ \t]+}}0x{{[0-9a-f]+}} [ra]
  // CHECK-NEXT: 1{{[ \t]+}}0x{{[0-9a-f]+}} [ra]
  // CHECK-NEXT: 2{{[ \t]+}}0x{{[0-9a-f]+}} [ra]
  // CHECK-NEXT: 3{{[ \t]+}}0x{{[0-9a-f]+}} [ra]
  // CHECK-NEXT: 4{{[ \t]+}}0x{{[0-9a-f]+}} [ra]
  // CHECK-NEXT: 5{{[ \t]+}}0x{{[0-9a-f]+}} [ra]

  print(backtrace)
}

@main
struct SimpleBacktrace {
  static func main() {
    level1()
  }
}
