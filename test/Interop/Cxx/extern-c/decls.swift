// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) %s -I %S/Inputs -import-bridging-header %S/Inputs/my-memory.h -enable-experimental-cxx-interop | %FileCheck %s


func zerome(ptr: UnsafeMutablePointer<Int>) {
  memset(ptr, 0, MemoryLayout<Int>.size)
}

// Verify that the asmname is "memset", not a C++-mangled version
// CHECK: sil [asmname "memset"] [clang memset] @$sSo6memsetySvSgAB_s5Int32VSitFTo : $@convention(c) (Optional<UnsafeMutableRawPointer>, Int32, Int) -> Optional<UnsafeMutableRawPointer>
