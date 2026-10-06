// RUN: %target-swift-frontend %s -emit-sil -O | %FileCheck %s --check-prefix=CHECK-SIL
// RUN: %target-swift-frontend %s -emit-ir -O | %FileCheck %s --check-prefix=CHECK-IR

// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib

// Check that the assume of Range's lowerBound <= upperBound invariant survives
// the SIL pipeline and reaches LLVM as llvm.assume, for each of the standard
// library entry points which state it.

// Array's range subscript, getter.
// CHECK-SIL-LABEL: sil [noinline] @$s12range_assume15arraySliceCountySiSaySiG_SnySiGtF :
// CHECK-SIL:         builtin "assume_Int1"
// CHECK-SIL:       } // end sil function '$s12range_assume15arraySliceCountySiSaySiG_SnySiGtF'

// CHECK-IR-LABEL: define{{.*}} @"$s12range_assume15arraySliceCountySiSaySiG_SnySiGtF"
// CHECK-IR:         [[CMP:%[0-9]+]] = icmp sge i64 %2, %1
// CHECK-IR:         call void @llvm.assume(i1 [[CMP]])
@inline(never)
public func arraySliceCount(_ a: [Int], _ r: Range<Int>) -> Int {
  return a[r].count
}

// ContiguousArray's range subscript, getter.
// CHECK-SIL-LABEL: sil [noinline] @$s12range_assume25contiguousArraySliceCountySis010ContiguousD0VySiG_SnySiGtF :
// CHECK-SIL:         builtin "assume_Int1"
// CHECK-SIL:       } // end sil function '$s12range_assume25contiguousArraySliceCountySis010ContiguousD0VySiG_SnySiGtF'
@inline(never)
public func contiguousArraySliceCount(_ a: ContiguousArray<Int>, _ r: Range<Int>) -> Int {
  return a[r].count
}

// ArraySlice's range subscript, getter.
// CHECK-SIL-LABEL: sil [noinline] @$s12range_assume010arraySliceD5CountySis05ArrayD0VySiG_SnySiGtF :
// CHECK-SIL:         builtin "assume_Int1"
// CHECK-SIL:       } // end sil function '$s12range_assume010arraySliceD5CountySis05ArrayD0VySiG_SnySiGtF'
@inline(never)
public func arraySliceSliceCount(_ a: ArraySlice<Int>, _ r: Range<Int>) -> Int {
  return a[r].count
}

public struct TestCollection: RandomAccessCollection {
  public var startIndex: Int { 0 }
  public var endIndex: Int { 100 }
  public subscript(i: Int) -> Int { i }
}

// The default Collection slicing subscript (SubSequence == Slice<Self>).
// CHECK-SIL-LABEL: sil [noinline] @$s12range_assume20collectionSliceCountySiAA14TestCollectionV_SnySiGtF :
// CHECK-SIL:         builtin "assume_Int1"
// CHECK-SIL:       } // end sil function '$s12range_assume20collectionSliceCountySiAA14TestCollectionV_SnySiGtF'
@inline(never)
public func collectionSliceCount(_ c: TestCollection, _ r: Range<Int>) -> Int {
  return c[r].count
}

// Slice's own range subscript: the second slicing goes through
// Slice.subscript(bounds:), the first through the Collection default.
// CHECK-SIL-LABEL: sil [noinline] @$s12range_assume17sliceOfSliceCountySiAA14TestCollectionV_SnySiGAEtF :
// CHECK-SIL:         builtin "assume_Int1"
// CHECK-SIL:         builtin "assume_Int1"
// CHECK-SIL:       } // end sil function '$s12range_assume17sliceOfSliceCountySiAA14TestCollectionV_SnySiGAEtF'
@inline(never)
public func sliceOfSliceCount(_ c: TestCollection, _ r: Range<Int>, _ q: Range<Int>) -> Int {
  return c[r][q].count
}

// Range's own startIndex/endIndex.
// CHECK-SIL-LABEL: sil [noinline] @$s12range_assume0A6BoundsySi_SitSnySiGF :
// CHECK-SIL:         builtin "assume_Int1"
// CHECK-SIL:       } // end sil function '$s12range_assume0A6BoundsySi_SitSnySiGF'

// CHECK-IR-LABEL: define{{.*}} @"$s12range_assume0A6BoundsySi_SitSnySiGF"
// CHECK-IR:         [[CMP:%[0-9]+]] = icmp sge i64 %1, %0
// CHECK-IR:         call void @llvm.assume(i1 [[CMP]])
@inline(never)
public func rangeBounds(_ r: Range<Int>) -> (Int, Int) {
  return (r.startIndex, r.endIndex)
}

// Array's range subscript, setter. The probe's body is moved into a specialized
// function; the assume must be in there.
// CHECK-SIL-LABEL: sil shared @$sSays10ArraySliceVyxGSnySiGcisSi_{{.*}} :
// CHECK-SIL:         builtin "assume_Int1"
// CHECK-SIL:       } // end sil function '$sSays10ArraySliceVyxGSnySiGcisSi_
@inline(never)
public func arraySliceSet(_ a: inout [Int], _ r: Range<Int>, _ v: ArraySlice<Int>) {
  a[r] = v
}
