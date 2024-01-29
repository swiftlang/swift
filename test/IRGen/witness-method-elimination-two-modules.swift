// Tests that under -enable-llvm-wme + -internalize-at-link, cross-module
// witness method calls are done via thunks and LLVM GlobalDCE is able to remove
// unused witness methods from a library based on a list of used symbols by a
// client.

// RUN: %empty-directory(%t)

// (1) Build library swiftmodule
// RUN: %target-build-swift -parse-as-library -Onone -Xfrontend -enable-llvm-wme \
// RUN:     %s -DLIBRARY -module-name Library \
// RUN:     -emit-module -o %t/Library.swiftmodule \
// RUN:     -emit-tbd -emit-tbd-path %t/libLibrary.tbd -Xfrontend -tbd-install_name=%t/libLibrary.dylib

// (2) Build client
// RUN: %target-build-swift -parse-as-library -Onone -Xfrontend -enable-llvm-wme \
// RUN:     %s -DCLIENT -module-name Main -I%t -L%t -lLibrary -o %t/main

// (3) Extract a list of used symbols by client from library
// RUN: %llvm-nm --undefined-only -m %t/main | grep 'from libLibrary' | awk '{print $3}' > %t/used-symbols

// (4) Now produce the .dylib with just the symbols needed by the client
// RUN: %target-build-swift -parse-as-library -Onone -Xfrontend -enable-llvm-wme -Xfrontend -internalize-at-link \
// RUN:     %s -DLIBRARY -lto=llvm-full %lto_flags -module-name Library \
// RUN:     -emit-library -o %t/libLibrary.dylib -runtime-compatibility-version none \
// RUN:     -Xlinker -exported_symbols_list -Xlinker %t/used-symbols -Xlinker -dead_strip

// (5) Check list of symbols in library
// RUN: %llvm-nm --defined-only %t/libLibrary.dylib | %FileCheck %s --check-prefix=NM

// (6) Execution test
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test

// FIXME(mracek): More work needed to get this to work on non-Apple platforms.
// REQUIRES: VENDOR=apple

// For LTO, the linker dlopen()'s the libLTO library, which is a scenario that
// ASan cannot work in ("Interceptors are not working, AddressSanitizer is
// loaded too late").
// REQUIRES: no_asan

// Remote test execution does not support dynamically loaded libraries.
// UNSUPPORTED: remote_run

#if LIBRARY

public protocol MyProtocol {
  func func1_used()
  func func2_unused()
}

public struct MyStruct : MyProtocol {
  public init() {}
  public func func1_used() { print("MyStruct.func1_used") }
  public func func2_unused() { print("MyStruct.func2_unused") }
}

// NM:     $s7Library8MyStructV10func1_usedyyF
// NM-NOT: $s7Library8MyStructV12func2_unusedyyF
// NM:     $s7Library8MyStructVAA0B8ProtocolA2aDP10func1_usedyyFTW
// NM-NOT: $s7Library8MyStructVAA0B8ProtocolA2aDP12func2_unusedyyFTW

#endif

#if CLIENT

import Library

@_cdecl("main")
func main() -> Int32 {
   let o: MyProtocol = MyStruct()
   o.func1_used()
   print("Done")
   // CHECK: MyStruct.func1_used
   // CHECK-NEXT: Done
   return 0
}

#endif
