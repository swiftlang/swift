// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature CopyBlockOptimization -O %s -emit-sil | %FileCheck %s

// REQUIRES: swift_feature_CopyBlockOptimization

func callClosure<R>(_ body: () -> R) -> R {
  return body()
}

// Check that after removing a copy_block, no retains+releases are inserted for the block.
// CHECK-LABEL: sil {{.*}}@testit :
// CHECK-NOT:     retain
// CHECK-NOT:     release
// CHECK:       } // end sil function 'testit'
@_cdecl("testit")
public func testit(_ block: (_ index: Int) -> Int) -> Bool {
  @inline(never)
  func c() -> Bool {
	  return block(0) != 0
  }

	return callClosure(c)
}
