// RUN: %empty-directory(%t)
// RUN: %target-clang -x c -Wno-unused-command-line-argument -Wl,--build-id -g %S/Inputs/fib.c -o %t/fib
// RUN: %target-clang -x c -Wno-unused-command-line-argument -g %S/Inputs/fib.c -o %t/fib-no-uuid
// RUN: %target-clang -x c -Wno-unused-command-line-argument -Wl,--build-id -Wl,--compress-debug-sections=zlib-gnu -g %S/Inputs/fib.c -o %t/fib-compress-gnu
// RUN: %target-clang -x c -Wno-unused-command-line-argument -Wl,--build-id -Wl,--compress-debug-sections=zlib -g %S/Inputs/fib.c -o %t/fib-compress-zlib
// RUN: %target-build-swift %s -parse-as-library -Xfrontend -disable-availability-checking -g -o %t/ElfReader
// RUN: %target-run %t/ElfReader %t/fib | %FileCheck %s
// RUN: %target-run %t/ElfReader %t/fib-no-uuid | %FileCheck %s --check-prefix NOUUID

// REQUIRES: OS=linux-gnu
// REQUIRES: backtracing

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

