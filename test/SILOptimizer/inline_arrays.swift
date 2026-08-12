// RUN: %target-swift-frontend  -primary-file %s -O -disable-availability-checking -module-name=test -emit-sil | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-OPT

// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib

// CHECK-LABEL: sil @$s4test0A9Subscriptys5UInt8Vs11InlineArrayVy$255_ADGz_SitF :
// CHECK-OPT:      [[S:%.*]] = struct_element_addr %0, #InlineArray._storage
// CHECK-ONONE:    [[ACC:%.*]] = begin_access [read] [static] %0
// CHECK-ONONE:    [[S:%.*]] = struct_element_addr [[ACC]], #InlineArray._storage
// CHECK:          [[BA:%.*]] = vector_base_addr [[S]]
// CHECK:          [[EA:%.*]] = index_addr [stack_protection] [projection] [[BA]],
// CHECK-OPT:      [[E:%.*]] = load [[EA]]
// CHECK-ONONE:    [[ACC2:%.*]] = begin_access [read] [unsafe] [[EA]]
// CHECK-ONONE:    [[E:%.*]] = load [[ACC2]]
// CHECK:          return [[E]]
// CHECK:       } // end sil function '$s4test0A9Subscriptys5UInt8Vs11InlineArrayVy$255_ADGz_SitF'
public func testSubscript(_ a: inout InlineArray<256, UInt8>, _ i: Int) -> UInt8 {
  return a[i]
}

public final class C {
  let a: InlineArray<256, UInt8>

  init(_ a: InlineArray<256, UInt8>) {
    self.a = a
  }

  // CHECK-LABEL:    sil @$s4test1CC1iACs5UInt8V_tcfc
  // CHECK-OPT-NOT:    alloc_stack
  // CHECK:       } // end sil function '$s4test1CC1iACs5UInt8V_tcfc'
  public init(i: UInt8) {
    self.a = .init(repeating: i)
  }

  // CHECK-LABEL: sil @$s4test1CC0A9Subscriptys5UInt8VSiF :
  // CHECK:          [[CA:%.*]] = ref_element_addr [immutable] %1, #C.a
  // CHECK:          [[S:%.*]] = struct_element_addr [[CA]], #InlineArray._storage
  // CHECK:          [[BA:%.*]] = vector_base_addr [[S]]
  // CHECK:          [[EA:%.*]] = index_addr [stack_protection] [projection] [[BA]],
  // CHECK-OPT:      [[E:%.*]] = load [[EA]]
  // CHECK-ONONE:    [[ACC2:%.*]] = begin_access [read] [unsafe] [[EA]]
  // CHECK-ONONE:    [[E:%.*]] = load [[ACC2]]
  // CHECK:          return [[E]]
  // CHECK:       } // end sil function '$s4test1CC0A9Subscriptys5UInt8VSiF'
  public func testSubscript(_ i: Int) -> UInt8 {
    return a[i]
  }
}

public struct S {
  let a: InlineArray<7000, UInt8>

  // CHECK-LABEL: sil @$s4test1SV0A9Subscriptys5UInt8VSiF :
  // CHECK:          [[A:%.*]] = struct_element_addr %1, #S.a
  // CHECK:          [[S:%.*]] = struct_element_addr [[A]], #InlineArray._storage
  // CHECK:          [[BA:%.*]] = vector_base_addr [[S]]
  // CHECK:          [[EA:%.*]] = index_addr [stack_protection] [projection] [[BA]],
  // CHECK-OPT:      [[E:%.*]] = load [[EA]]
  // CHECK-ONONE:    [[ACC2:%.*]] = begin_access [read] [unsafe] [[EA]]
  // CHECK-ONONE:    [[E:%.*]] = load [[ACC2]]
  // CHECK:          return [[E]]
  // CHECK:       } // end sil function '$s4test1SV0A9Subscriptys5UInt8VSiF'
  public func testSubscript(_ i: Int) -> UInt8 {
    return a[i]
  }
}

// rdar://172132077 (Iterating over InlineArray copies the full array per iteration)
//
// The InlineArray is stored on the stack exactly once, and that allocation is
// used in every iteration. Once we have large loadable address lowering, large
// InlineArrays will be passed indirectly in SIL, allowing us to eliminate the
// single top-level copy that remains.
//
// CHECK-LABEL: sil @$s4test25dontCopyEveryIterationBig1as5Int32Vs11InlineArrayVy$499_AEG_tF : $@convention(thin) (InlineArray<500, Int32>) -> Int32 {
// CHECK:       bb0(%0 : $InlineArray<500, Int32>):
// CHECK:         [[STK:%[0-9]+]] = alloc_stack
// CHECK:         store %0 to [[STK]]
// CHECK:       bb1:
// CHECK-NOT:     = alloc_stack
// CHECK-NOT:     store
// CHECK:       } // end sil function '$s4test25dontCopyEveryIterationBig1as5Int32Vs11InlineArrayVy$499_AEG_tF'
public func dontCopyEveryIterationBig(a: [500 of Int32]) -> Int32 {
  var s: Int32 = 0
  for i in a.indices {
    s += a[i]
  }
  return s
}

// CHECK-LABEL: sil @$s4test31dontCopyEveryIterationBigBorrow1as5Int32Vs11InlineArrayVy$499_AEG_tF : $@convention(thin) (InlineArray<500, Int32>) -> Int32 {
// CHECK:       bb0(%0 : @noImplicitCopy $InlineArray<500, Int32>):
// CHECK:         [[STK:%[0-9]+]] = alloc_stack
// CHECK:         store {{.*}} to [[STK]]
// CHECK:       {{^}}bb1
// CHECK-NOT:     = alloc_stack
// CHECK-NOT:     store
// CHECK:       } // end sil function '$s4test31dontCopyEveryIterationBigBorrow1as5Int32Vs11InlineArrayVy$499_AEG_tF'
public func dontCopyEveryIterationBigBorrow(a: borrowing [500 of Int32]) -> Int32 {
  var s: Int32 = 0
  for i in a.indices {
    s += a[i]
  }
  return s
}

// Take an array of indices to prevent loop unrolling.
// CHECK-LABEL: sil @$s4test27dontCopyEveryIterationSmall1a7indicess5Int32Vs11InlineArrayVy$1_AFG_SayAFGtF : $@convention(thin) (InlineArray<2, Int32>, @guaranteed Array<Int32>) -> Int32 {
// CHECK:       bb0(%0 : $InlineArray<2, Int32>, %1 : $Array<Int32>):
// CHECK:       {{^}}bb1
// CHECK:         [[STK:%[0-9]+]] = alloc_stack
// CHECK:         store {{.*}} to [[STK]]
// CHECK:       {{^}}bb2
// CHECK-NOT:     = alloc_stack
// CHECK-NOT:     store
// CHECK:       } // end sil function '$s4test27dontCopyEveryIterationSmall1a7indicess5Int32Vs11InlineArrayVy$1_AFG_SayAFGtF'
public func dontCopyEveryIterationSmall(a: [2 of Int32], indices: [Int32]) -> Int32 {
  var s: Int32 = 0
  for i in indices {
    s += a[Int(i)]
  }
  return s
}

// Speculative store hoisting.
// CHECK-LABEL: sil @$s4test36dontCopyEveryIterationBigConditional1a1fs5Int32Vs11InlineArrayVy$499_AFG_SbSiXEtF : $@convention(thin) (InlineArray<500, Int32>, @guaranteed @noescape @callee_guaranteed (Int) -> Bool) -> Int32 {
// CHECK:       bb0(%0 : $InlineArray<500, Int32>, %1 : $@noescape @callee_guaranteed (Int) -> Bool):
// CHECK:         [[STK:%[0-9]+]] = alloc_stack
// CHECK:         store %0 to [[STK]]
// CHECK:       {{^}}bb1
// CHECK:       } // end sil function '$s4test36dontCopyEveryIterationBigConditional1a1fs5Int32Vs11InlineArrayVy$499_AFG_SbSiXEtF'
public func dontCopyEveryIterationBigConditional(a: [500 of Int32], f: (Int) -> Bool) -> Int32 {
  var s: Int32 = 0
  for i in a.indices {
    if f(i) {
      s += a[i]
    }
  }
  return s
}

// TODO: Speculatively hoist the store in this case, where the access base is outside the loop.
// 
// CHECK-LABEL: sil @$s4test38dontCopyEveryIterationSmallConditional1a7indices1fs5Int32Vs11InlineArrayVy$1_AGG_SayAGGSbSiXEtF : $@convention(thin) (InlineArray<2, Int32>, @guaranteed Array<Int32>, @guaranteed @noescape @callee_guaranteed (Int) -> Bool) -> Int32 {
// CHECK:       bb0(%0 : $InlineArray<2, Int32>, %1 : $Array<Int32>, %2 : $@noescape @callee_guaranteed (Int) -> Bool):
// CHECK:       bb1:
// CHECK:       bb6:
// CHECK:         alloc_stack
// CHECK:         store
// CHECK:       bb7:
// CHECK:       } // end sil function '$s4test38dontCopyEveryIterationSmallConditional1a7indices1fs5Int32Vs11InlineArrayVy$1_AGG_SayAGGSbSiXEtF'
public func dontCopyEveryIterationSmallConditional(a: [2 of Int32], indices: [Int32], f: (Int) -> Bool) -> Int32 {
  var s: Int32 = 0
  for i in indices {
    if f(Int(i)) {
      s += a[Int(i)]
    }
  }
  return s
}

// TODO: Eliminate the redundant store in this case, where the loop is unrolled.
//
// CHECK-LABEL: sil @$s4test46dontCopyEveryIterationSmallConditionalUnrolled1a1fs5Int32Vs11InlineArrayVy$1_AFG_SbSiXEtF : $@convention(thin) (InlineArray<2, Int32>, @guaranteed @noescape @callee_guaranteed (Int) -> Bool) -> Int32 {
// CHECK:         alloc_stack
// CHECK:         store
// CHECK:         store
// CHECK:         dealloc_stack
// CHECK-NOT:     alloc_stack
// CHECK-NOT:     store
// CHECK-NOT:     dealloc_stack
// CHECK:       } // end sil function '$s4test46dontCopyEveryIterationSmallConditionalUnrolled1a1fs5Int32Vs11InlineArrayVy$1_AFG_SbSiXEtF'
public func dontCopyEveryIterationSmallConditionalUnrolled(a: [2 of Int32], f: (Int) -> Bool) -> Int32 {
  var s: Int32 = 0
  for i in a.indices {
    if f(i) {
      s += a[i]
    }
  }
  return s
}

// CHECK-LABEL: sil @$s4test5equalySbs11InlineArrayVy$31_SiG_AEtF : $@convention(thin) (InlineArray<32, Int>, InlineArray<32, Int>) -> Bool {
// CHECK:       bb0(%0 : @noImplicitCopy $InlineArray<32, Int>, %1 : @noImplicitCopy $InlineArray<32, Int>):
// CHECK:         [[STK1:%[0-9]+]] = alloc_stack
// CHECK:         store {{.*}} to [[STK1]]
// CHECK:         [[STK2:%[0-9]+]] = alloc_stack
// CHECK:         store {{.*}} to [[STK2]]
// CHECK:       {{^}}bb2
// CHECK-NOT:     store
// CHECK:       } // end sil function '$s4test5equalySbs11InlineArrayVy$31_SiG_AEtF'
public func equal(_ lhs: borrowing [32 of Int], _ rhs: borrowing [32 of Int]) -> Bool {
    for i in 0..<32 {
        guard lhs[i] == rhs[i] else {
            return false
        }
    }
    return true
}
