// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Xfrontend -disable-availability-checking -g -o %t/ElfReader
// RUN: %target-run %t/ElfReader %S/Inputs/fib | %FileCheck %s
// RUN: %target-run %t/ElfReader %S/Inputs/fib-compress-gnu | %FileCheck %s --check-prefix COMPRESS-GNU
// RUN: %target-run %t/ElfReader %S/Inputs/fib-compress-zlib | %FileCheck %s --check-prefix COMPRESS-ZLIB
// RUN: %target-run %t/ElfReader %S/Inputs/fib-no-uuid | %FileCheck %s --check-prefix NOUUID

// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: backtracing
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

@_spi(ElfTest) import Runtime
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
struct ElfReader {

  static func main() {
    if CommandLine.argc != 2 {
      print("usage: ElfReader <path-to-binary>")
      return
    }

    // CHECK: {{.*}}/fib is a {{(32|64)}}-bit ELF image
    // CHECK-NEXT:   uuid: {{[0-9a-f]+}}
    // CHECK-NEXT:   .debug_info: found
    // CHECK-NEXT:   .debug_line: found
    // CHECK-NEXT:   .debug_abbrev: found
    // CHECK-NEXT:   .debug_ranges: not found
    // CHECK-NEXT:   .debug_str: found
    // CHECK-NEXT:   .debug_addr: found
    // CHECK-NEXT:   .debug_str_offsets: found
    // CHECK-NEXT:   .debug_line_str: found
    // CHECK-NEXT:   .debug_rnglists: not found

    // COMPRESS-GNU: {{.*}}/fib-compress-gnu is a {{(32|64)}}-bit ELF image
    // COMPRESS-GNU-NEXT:   uuid: {{[0-9a-f]+}}
    // COMPRESS-GNU-NEXT:   .debug_info: found
    // COMPRESS-GNU-NEXT:   .debug_line: found
    // COMPRESS-GNU-NEXT:   .debug_abbrev: found
    // COMPRESS-GNU-NEXT:   .debug_ranges: not found
    // COMPRESS-GNU-NEXT:   .debug_str: found
    // COMPRESS-GNU-NEXT:   .debug_addr: found
    // COMPRESS-GNU-NEXT:   .debug_str_offsets: found
    // COMPRESS-GNU-NEXT:   .debug_line_str: found
    // COMPRESS-GNU-NEXT:   .debug_rnglists: not found

    // COMPRESS-ZLIB: {{.*}}/fib-compress-zlib is a {{(32|64)}}-bit ELF image
    // COMPRESS-ZLIB-NEXT:   uuid: {{[0-9a-f]+}}
    // COMPRESS-ZLIB-NEXT:   .debug_info: found
    // COMPRESS-ZLIB-NEXT:   .debug_line: found
    // COMPRESS-ZLIB-NEXT:   .debug_abbrev: found
    // COMPRESS-ZLIB-NEXT:   .debug_ranges: not found
    // COMPRESS-ZLIB-NEXT:   .debug_str: found
    // COMPRESS-ZLIB-NEXT:   .debug_addr: found
    // COMPRESS-ZLIB-NEXT:   .debug_str_offsets: found
    // COMPRESS-ZLIB-NEXT:   .debug_line_str: found
    // COMPRESS-ZLIB-NEXT:   .debug_rnglists: not found

    // NOUUID: {{.*}}/fib-no-uuid is a {{(32|64)}}-bit ELF image
    // NOUUID-NEXT:   uuid: <no uuid>
    // NOUUID-NEXT:   .debug_info: found
    // NOUUID-NEXT:   .debug_line: found
    // NOUUID-NEXT:   .debug_abbrev: found
    // NOUUID-NEXT:   .debug_ranges: not found
    // NOUUID-NEXT:   .debug_str: found
    // NOUUID-NEXT:   .debug_addr: found
    // NOUUID-NEXT:   .debug_str_offsets: found
    // NOUUID-NEXT:   .debug_line_str: found
    // NOUUID-NEXT:   .debug_rnglists: not found

    if !testElfImageAt(path: CommandLine.arguments[1]) {
      exit(1)
    }
  }

}

