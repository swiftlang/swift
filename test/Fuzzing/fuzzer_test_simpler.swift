// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library -sanitize=fuzzer,address -c %s -o %S/file.o
// RUN: %clang %target-cc-options -isysroot %sdk -fobjc-arc -fsanitize-coverage=edge,trace-pc-guard -c %S/objcwrapper.m -c -o %S/objcwrapper.o
// RUN: %target-swiftc_driver -sanitize=fuzzer,address %S/objcwrapper.o %S/file.o -o %S/fuzztarget
// RUN: not %S/fuzztarget -only_ascii=1 -max_len=3 | %FileCheck -check-prefix=CRASH %s
// CRASH: Crash!

// Cleanup.
// RUN: rm -f %S/*.o %S/fuzztarget

// REQUIRES: objc_interop

import Foundation

@objc(Fuzzer) class Fuzzer : NSObject {
  @objc static func fuzzOneInput(Data: UnsafePointer<CChar>, Size: CLong) {
    if (Size >= 3) {
      if (Data[0] == 65) {
        if (Data[1] == 66) {
          if (Data[2] == 67) {
            print("Crash!");
            assert(false);
          }
        }
      }
    }
  }
}
