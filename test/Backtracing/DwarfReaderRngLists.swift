// RUN: %empty-directory(%t)
// RUN: %yaml2obj %S/Inputs/DwarfRngLists.yaml -o %t/rnglists.o
// RUN: %target-build-swift %s -parse-as-library -g -o %t/DwarfReaderRngLists
// RUN: %target-run %t/DwarfReaderRngLists %t/rnglists.o | %FileCheck %s

// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: backtracing
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Regression test for DwarfReader's DWARF v5 .debug_rnglists support
// (DW_FORM_rnglistx / DW_AT_ranges). Inputs/DwarfRngLists.yaml is a
// hand-crafted object exercising every DW_RLE_* range list encoding,
// including discontiguous ranges on both a DW_TAG_subprogram and several
// DW_TAG_inlined_subroutine DIEs.

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
struct DwarfReaderRngLists {
  static func main() {
    if CommandLine.argc != 2 {
      print("usage: DwarfReaderRngLists <path-to-binary>")
      return
    }

    // CHECK: is a 64-bit ELF image
    // CHECK: Units:

    // rangelist 0 (DW_RLE_base_address + two DW_RLE_offset_pair), attached
    // directly to the enclosing DW_TAG_subprogram's DW_AT_ranges.
    // CHECK: Functions:
    // CHECK: lowPC: 4112, highPC: 4128
    // CHECK-SAME: lowPC: 4144, highPC: 4160

    // CHECK: Call Sites:

    // rangelist 3 (DW_RLE_startx_length, .debug_addr[0] = 0x2000)
    // CHECK: lowPC: 8192, highPC: 8200
    // CHECK-SAME: line: 30

    // rangelist 1 (DW_RLE_base_addressx + two discontiguous
    // DW_RLE_offset_pair -- the original bug scenario)
    // CHECK-SAME: lowPC: 8196, highPC: 8200
    // CHECK-SAME: line: 10
    // CHECK-SAME: lowPC: 8208, highPC: 8216
    // CHECK-SAME: line: 10

    // rangelist 2 (DW_RLE_startx_endx, .debug_addr[1..2])
    // CHECK-SAME: lowPC: 12288, highPC: 12304
    // CHECK-SAME: line: 20

    // rangelist 4 (DW_RLE_base_address + DW_RLE_offset_pair)
    // CHECK-SAME: lowPC: 36865, highPC: 36866
    // CHECK-SAME: line: 40

    // rangelist 5 (DW_RLE_start_end + discontiguous DW_RLE_start_length)
    // CHECK-SAME: lowPC: 40960, highPC: 40976
    // CHECK-SAME: line: 50
    // CHECK-SAME: lowPC: 45056, highPC: 45072
    // CHECK-SAME: line: 50

    if !testDwarfReaderFor(path: CommandLine.arguments[1]) {
      exit(1)
    }
  }
}
