// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -target %target-cpu-macos15.0 -Xfrontend -parse-as-library -Onone -o %t/CodableBacktrace
// RUN: %target-codesign %t/CodableBacktrace
// RUN: %target-run %t/CodableBacktrace | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Runtime
import Foundation

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

  let encoder = JSONEncoder()
  encoder.outputFormatting = [.prettyPrinted,.sortedKeys,.withoutEscapingSlashes]

  let data = try! encoder.encode(backtrace)
  let json = String(data: data, encoding: .utf8)!

  print(json)

  // CHECK: {
  // CHECK:    "architecture" : "{{.*}}",
  // CHECK:    "backtrace" : "{{[A-Za-z0-9+/]*}}"
  // CHECK: }

  let decoder = JSONDecoder()

  let bt2 = try! decoder.decode(Backtrace.self, from: data)

  print(bt2)

  // CHECK:      0{{[ \t]+}}0x{{[0-9a-f]+}} [ra]
  // CHECK-NEXT: 1{{[ \t]+}}0x{{[0-9a-f]+}} [ra]
  // CHECK-NEXT: 2{{[ \t]+}}0x{{[0-9a-f]+}} [ra]
  // CHECK-NEXT: 3{{[ \t]+}}0x{{[0-9a-f]+}} [ra]
  // CHECK-NEXT: 4{{[ \t]+}}0x{{[0-9a-f]+}} [ra]
  // CHECK-NEXT: 5{{[ \t]+}}0x{{[0-9a-f]+}} [ra]
}

@main
struct CodableBacktrace {
  static func main() {
    level1()
  }
}
