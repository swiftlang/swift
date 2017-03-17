// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -O -emit-sil -parse-as-library %s | %FileCheck %s

// Test the absence of a 'strict' flag.
// CHECK-LABEL: _T018unsafe_pointer_gen13test_raw_loadSiSV2rp_tF
// CHECK: pointer_to_address {{%.*}} : $Builtin.RawPointer to $*Int
public func test_raw_load(rp: UnsafeRawPointer) -> Int {
  return rp.load(as: Int.self)
}
