// RUN: %empty-directory(%t)
// RUN: %yaml2obj %S/Inputs/DwarfSLEB128Overflow.yaml -o %t/sleb128.o
// RUN: %target-build-swift %s -parse-as-library -g -o %t/DwarfReaderSLEB128Overflow
// RUN: %target-run %t/DwarfReaderSLEB128Overflow %t/sleb128.o | %FileCheck %s

// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: backtracing
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Regression test for fetchSLEB128 (Dwarf.swift): decoding a 9-byte SLEB128
// value whose final continuation byte leaves the running shift at exactly
// 63 with the sign bit set used to trap while sign-extending, because
// `-(1 << 63)` overflows Int64. Inputs/DwarfSLEB128Overflow.yaml encodes
// DW_AT_const_value using exactly that byte pattern.

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
struct DwarfReaderSLEB128Overflow {
  static func main() {
    if CommandLine.argc != 2 {
      print("usage: DwarfReaderSLEB128Overflow <path-to-binary>")
      return
    }

    // CHECK: is a 64-bit ELF image
    // CHECK: Units:

    if !testDwarfReaderFor(path: CommandLine.arguments[1]) {
      exit(1)
    }
  }
}
