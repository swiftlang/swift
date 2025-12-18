// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Xfrontend -disable-availability-checking -g -Onone -o %t/SymbolicatedBacktrace.exe
// RUN: %target-codesign %t/SymbolicatedBacktrace.exe
// RUN: %target-run %t/SymbolicatedBacktrace.exe | %FileCheck %s

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx || OS=linux-gnu || OS=windows-msvc

import Runtime

func kablam() {
  kerpow()
}

func kerpow() {
  whap()
}

func whap() {
  zonk()
}

func zonk() {
  splat()
}

func splat() {
  pow()
}

func pow() {
  let backtrace = try! Backtrace.capture().symbolicated(
    options: [ .showInlineFrames, .showSourceLocations ]
  )!

  // CHECK:      0{{[ \t]+}}0x{{[0-9a-f]+}} [ra] [0] SymbolicatedBacktrace.exe pow()
  // CHECK:      1{{[ \t]+}}0x{{[0-9a-f]+}} [ra] [0] SymbolicatedBacktrace.exe splat()
  // CHECK:      2{{[ \t]+}}0x{{[0-9a-f]+}} [ra] [0] SymbolicatedBacktrace.exe zonk()
  // CHECK:      3{{[ \t]+}}0x{{[0-9a-f]+}} [ra] [0] SymbolicatedBacktrace.exe whap()
  // CHECK:      4{{[ \t]+}}0x{{[0-9a-f]+}} [ra] [0] SymbolicatedBacktrace.exe kerpow()
  // CHECK:      5{{[ \t]+}}0x{{[0-9a-f]+}} [ra] [0] SymbolicatedBacktrace.exe kablam()
  // CHECK:      6{{[ \t]+}}0x{{[0-9a-f]+}} [ra] [0] SymbolicatedBacktrace.exe static SymbolicatedBacktrace.main()

  print(backtrace)
}

@main
struct SymbolicatedBacktrace {
  static func main() {
    kablam()
  }
}
