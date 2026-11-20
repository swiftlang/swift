// RUN: %empty-directory(%t)
// RUN: %yaml2obj %S/Inputs/DwarfLineMaxOpsZero.yaml -o %t/maxops.o
// RUN: %target-build-swift %s -parse-as-library -g -o %t/DwarfReaderLineMaxOpsZero
// RUN: %target-run %t/DwarfReaderLineMaxOpsZero %t/maxops.o | %FileCheck %s

// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: backtracing
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Regression test for rdar://191398178: a DWARF 4+ line number program header
// with maximum_operations_per_instruction == 0 used to cause a division by
// zero trap when the line number program was executed.  The header parser now
// treats a zero value as 1.  Inputs/DwarfLineMaxOpsZero.yaml is a hand-crafted
// object with exactly such a header.

@_spi(DwarfTest) import Runtime
#if canImport(Darwin)
import Darwin
#elseif canImport(SwiftWASILibc)
import SwiftWASILibc
#elseif canImport(ucrt)
import ucrt
#elseif canImport(SwiftGlibc)
import SwiftGlibc
#endif

@main
struct DwarfReaderLineMaxOpsZero {
  static func main() {
    if CommandLine.argc != 2 {
      print("usage: DwarfReaderLineMaxOpsZero <path-to-binary>")
      return
    }

    // CHECK: is a 64-bit ELF image
    // CHECK: Line Program:
    // CHECK-NEXT: executed

    if !testDwarfReaderFor(path: CommandLine.arguments[1]) {
      exit(1)
    }
  }
}
