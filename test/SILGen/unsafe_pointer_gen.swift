// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -O -parse-as-library %s | %FileCheck %s

// Test the absence of a 'strict' flag.
// Test the absence of an 'align' flag.
//
// CHECK-LABEL: $s18unsafe_pointer_gen13test_raw_load2rpSiSV_tF
// CHECK: pointer_to_address {{%.*}} : $Builtin.RawPointer to $*Int
public func test_raw_load(rp: UnsafeRawPointer) -> Int {
  return rp.load(as: Int.self)
}

// Test the absence of a 'strict' flag.
// Test the absence of an 'align' flag.
//
// CHECK-LABEL: $s18unsafe_pointer_gen20test_mutableraw_load2rpSiSv_tF
// CHECK: pointer_to_address {{%.*}} : $Builtin.RawPointer to $*Int
public func test_mutableraw_load(rp: UnsafeMutableRawPointer) -> Int {
  return rp.load(as: Int.self)
}
