// RUN: %target-swift-frontend -O -emit-sil -parse-as-library %s | %FileCheck %s

// FIXME: https://bugs.swift.org/browse/SR-2808
// XFAIL: resilient_stdlib

// Test the absence of a 'strict' flag.
// CHECK-LABEL: _TF18unsafe_pointer_gen13test_raw_loadFT2rpSV_Si
// CHECK: pointer_to_address {{%.*}} : $Builtin.RawPointer to $*Int
public func test_raw_load(rp: UnsafeRawPointer) -> Int {
  return rp.load(as: Int.self)
}
