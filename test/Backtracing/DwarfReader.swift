// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -g -o %t/DwarfReader
// RUN: %target-run %t/DwarfReader %S/Inputs/Inlining | %FileCheck %s

// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: backtracing

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
struct DwarfReader {
  static func main() {
    if CommandLine.argc != 2 {
      print("usage: DwarfReader <path-to-binary>")
      return
    }

    // CHECK: {{.*}}/Inlining is a {{32|64}}-bit ELF image
    // CHECK: Units:
    // CHECK: Call Sites:

    if !testDwarfReaderFor(path: CommandLine.arguments[1]) {
      exit(1)
    }
  }
}
