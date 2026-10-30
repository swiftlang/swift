// RUN: %empty-directory(%t)
// RUN: %yaml2obj %S/Inputs/DwarfCrossUnitRef.yaml -o %t/crossunit.o
// RUN: %target-build-swift %s -parse-as-library -g -o %t/DwarfReaderCrossUnitRef
// RUN: %target-run %t/DwarfReaderCrossUnitRef %t/crossunit.o | %FileCheck %s

// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: backtracing
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Regression test for DwarfReader following a DW_AT_abstract_origin reference
// into a different unit (as produced by `dwz`).  The referenced DIE has to be
// decoded using the abbreviations of the unit that contains it, not those of
// the referring unit; otherwise we misparse it (and may crash).
// Inputs/DwarfCrossUnitRef.yaml is a hand-crafted object with a partial unit
// and a compile unit that use different abbreviation tables.

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
struct DwarfReaderCrossUnitRef {
  static func main() {
    if CommandLine.argc != 2 {
      print("usage: DwarfReaderCrossUnitRef <path-to-binary>")
      return
    }

    // CHECK: is a 64-bit ELF image
    // CHECK: Units:

    // CHECK: Call Sites:
    // CHECK: rawName: Optional("callee")
    // CHECK-SAME: lowPC: 4096, highPC: 4112
    // CHECK-SAME: line: 7

    if !testDwarfReaderFor(path: CommandLine.arguments[1]) {
      exit(1)
    }
  }
}
