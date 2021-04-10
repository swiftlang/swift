// RUN: %target-build-swift -parse-as-library -sanitize=fuzzer %s -o %t
// RUN: not %t -only_ascii=1 -max_len=3 | %FileCheck %s
// REQUIRES: CPU=x86_64
// REQUIRES: executable_test
// REQUIRES: fuzzer_runtime
// XFAIL: OS=ios
// XFAIL: OS=tvos
// XFAIL: OS=watchos
// CHECK: Crash!

#if canImport(Darwin)
import Darwin.C
#elseif canImport(Glibc)
import Glibc
#elseif canImport(MSVCRT)
import MSVCRT
#endif

@_cdecl("LLVMFuzzerTestOneInput")
public func testHexDigits(_ start: UnsafeRawPointer, _ count: Int) -> CInt {
  let bytes = UnsafeRawBufferPointer(start: start, count: count)
  let string = String(decoding: bytes, as: Unicode.UTF8.self)
  if let number = Int(string, radix: 16), (0x10...0xFF).contains(number) {
    print("Crash!")
    fflush(stdout)
    exit(EXIT_FAILURE)
  }
  return 0
}
