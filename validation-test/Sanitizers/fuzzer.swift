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
public func test(_ start: UnsafeRawPointer, _ count: Int) -> CInt {
  let bytes = UnsafeRawBufferPointer(start: start, count: count)
  if bytes.starts(with: "ABC".utf8) {
    print("Crash!")
    fflush(stdout)
    exit(EXIT_FAILURE)
  }
  return 0
}
