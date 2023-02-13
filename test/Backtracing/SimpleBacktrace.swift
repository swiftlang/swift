// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Xfrontend -parse-as-library -Onone -o %t/SimpleBacktrace
// RUN: %target-codesign %t/SimpleBacktrace
// RUN: %target-run %t/SimpleBacktrace | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=iOS || OS=watchOS || OS=tvOS

import _Backtracing

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
