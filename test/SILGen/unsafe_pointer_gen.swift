// RUN: %target-swift-emit-sil -O -parse-as-library %s | %FileCheck %s

// Test the absence of a 'strict' flag.
// CHECK-LABEL: $S18unsafe_pointer_gen13test_raw_load2rpSiSV_tF
// CHECK: pointer_to_address {{%.*}} : $Builtin.RawPointer to $*Int
public func test_raw_load(rp: UnsafeRawPointer) -> Int {
  return rp.load(as: Int.self)
}
