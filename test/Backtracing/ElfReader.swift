// RUN: %empty-directory(%t)
// RUN: %target-clang -x c -Wl,--build-id -g %S/Inputs/fib.c -o %t/fib
// RUN: %target-clang -x c -g %S/Inputs/fib.c -o %t/fib-no-uuid
// RUN: %target-clang -x c -Wl,--build-id -Wl,--compress-debug-sections=zlib-gnu -g %S/Inputs/fib.c -o %t/fib-compress-gnu
// RUN: %target-clang -x c -Wl,--build-id -Wl,--compress-debug-sections=zlib -g %S/Inputs/fib.c -o %t/fib-compress-zlib
// RUN: %target-build-swift %s -parse-as-library -g -o %t/ElfReader
// RUN: %target-run %t/ElfReader %t/fib || %FileCheck %s
// RUN: %target-run %t/ElfReader %t/fib-no-uuid || %FileCheck %s --check-prefix NOUUID
// RUN: %target-run %t/ElfReader %t/fib-compress-gnu || %FileCheck %s --check-prefix CMPGNU
// RUN: %target-run %t/ElfReader %t/fib-compress-zlib || %FileCheck %s --check-prefix CMPZLIB

// REQUIRES: OS=linux-gnu

@_spi(ElfTest) import _Backtracing

@main
struct ElfReader {

  static func main() {
    if CommandLine.argc != 2 {
      print("usage: ElfReader <path-to-binary>")
      return
    }

    // CHECK: {{.*}}/fib is a {{(32|64)}}-bit ELF image
    // CHECK:   uuid: {{[0-9a-f]+}}
    // CHECK:   debug image: <none>
    // CHECK:   .debug_info: found
    // CHECK:   .debug_line: found
    // CHECK:   .debug_abbrev: found
    // CHECK:   .debug_ranges: not found
    // CHECK:   .debug_str: found
    // CHECK:   .debug_addr: found
    // CHECK:   .debug_str_offsets: found
    // CHECK:   .debug_line_str: found
    // CHECK:   .debug_rnglists: not found

    // NOUUID: {{.*}}/fib-no-uuid is a {{(32|64)}}-bit ELF image
    // NOUUID:   uuid: <no uuid>
    // NOUUID:   debug image: <none>
    // NOUUID:   .debug_info: found
    // NOUUID:   .debug_line: found
    // NOUUID:   .debug_abbrev: found
    // NOUUID:   .debug_ranges: not found
    // NOUUID:   .debug_str: found
    // NOUUID:   .debug_addr: found
    // NOUUID:   .debug_str_offsets: found
    // NOUUID:   .debug_line_str: found
    // NOUUID:   .debug_rnglists: not found

    // CMPGNU: {{.*}}/fib is a {{(32|64)}}-bit ELF image
    // CMPGNU:   uuid: {{[0-9a-f]+}}
    // CMPGNU:   debug image: <none>
    // CMPGNU:   .debug_info: found
    // CMPGNU:   .debug_line: found
    // CMPGNU:   .debug_abbrev: found
    // CMPGNU:   .debug_ranges: not found
    // CMPGNU:   .debug_str: found
    // CMPGNU:   .debug_addr: found
    // CMPGNU:   .debug_str_offsets: found
    // CMPGNU:   .debug_line_str: found
    // CMPGNU:   .debug_rnglists: not found

    // CMPZLIB: {{.*}}/fib is a {{(32|64)}}-bit ELF image
    // CMPZLIB:   uuid: {{[0-9a-f]+}}
    // CMPZLIB:   debug image: <none>
    // CMPZLIB:   .debug_info: found
    // CMPZLIB:   .debug_line: found
    // CMPZLIB:   .debug_abbrev: found
    // CMPZLIB:   .debug_ranges: not found
    // CMPZLIB:   .debug_str: found
    // CMPZLIB:   .debug_addr: found
    // CMPZLIB:   .debug_str_offsets: found
    // CMPZLIB:   .debug_line_str: found
    // CMPZLIB:   .debug_rnglists: not found

    if !testElfImageAt(path: CommandLine.arguments[1]) {
      exit(1)
    }
  }

}

