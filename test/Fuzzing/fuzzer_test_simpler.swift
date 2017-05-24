// RUN: %target-build-swift -parse-as-library -sanitize=fuzzer %s -o %t
// RUN: not %t -only_ascii=1 -max_len=3 | %FileCheck %s
// CHECK: Crash!

import Foundation

@_cdecl("LLVMFuzzerTestOneInput") public func fuzzOneInput(Data: UnsafePointer<CChar>, Size: CLong) -> CInt {
  if (Size >= 3) {
    if (Data[0] == 65) {
      if (Data[1] == 66) {
        if (Data[2] == 67) {
          print("Crash!");
          exit(1);
        }
      }
    }
  }
  return 0;
}
