// RUN: %target-swift-frontend %s -emit-sil -O | %FileCheck %s --check-prefix=CHECK-SIL
// RUN: %target-swift-frontend %s -emit-ir -O -disable-llvm-merge-functions-pass | %FileCheck %s --check-prefix=CHECK-IR

// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib

// Check that the assume of Range's lowerBound <= upperBound invariant survives
// the SIL pipeline and reaches LLVM as llvm.assume, for each of the standard
// library entry points which state it.

// Array's range subscript, getter.
// CHECK-SIL-LABEL: sil [noinline] @$s12range_assume15arraySliceCountySiSaySiG_SnySiGtF :
// CHECK-SIL:         builtin "assumeLessThanOrEqualTo"
// CHECK-SIL:       } // end sil function '$s12range_assume15arraySliceCountySiSaySiG_SnySiGtF'

// LLVM uses it: with lowerBound >= 0 checked, upperBound >= 0 needs no check,
// so three of the four bounds checks remain.
// CHECK-IR-LABEL: define{{.*}} @"$s12range_assume15arraySliceCountySiSaySiG_SnySiGtF"
// CHECK-IR:         [[CMP:%[0-9]+]] = icmp sle i{{32|64}} %1, %2
// CHECK-IR:         call void @llvm.assume(i1 [[CMP]])
// CHECK-IR-COUNT-3: call void @llvm.trap()
// CHECK-IR-NOT:     call void @llvm.trap()
// CHECK-IR:         {{^}}}
@inline(never)
public func arraySliceCount(_ a: [Int], _ r: Range<Int>) -> Int {
  return a[r].count
}

// ContiguousArray's range subscript, getter.
// CHECK-SIL-LABEL: sil [noinline] @$s12range_assume25contiguousArraySliceCountySis010ContiguousD0VySiG_SnySiGtF :
// CHECK-SIL:         builtin "assumeLessThanOrEqualTo"
// CHECK-SIL:       } // end sil function '$s12range_assume25contiguousArraySliceCountySis010ContiguousD0VySiG_SnySiGtF'
@inline(never)
public func contiguousArraySliceCount(_ a: ContiguousArray<Int>, _ r: Range<Int>) -> Int {
  return a[r].count
}

// ArraySlice's range subscript, getter.
// CHECK-SIL-LABEL: sil [noinline] @$s12range_assume010arraySliceD5CountySis05ArrayD0VySiG_SnySiGtF :
// CHECK-SIL:         builtin "assumeLessThanOrEqualTo"
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
// CHECK-SIL:         builtin "assumeLessThanOrEqualTo"
// CHECK-SIL:       } // end sil function '$s12range_assume20collectionSliceCountySiAA14TestCollectionV_SnySiGtF'
@inline(never)
public func collectionSliceCount(_ c: TestCollection, _ r: Range<Int>) -> Int {
  return c[r].count
}

// Slice's own range subscript: the second slicing goes through
// Slice.subscript(bounds:), the first through the Collection default.
// CHECK-SIL-LABEL: sil [noinline] @$s12range_assume17sliceOfSliceCountySiAA14TestCollectionV_SnySiGAEtF :
// CHECK-SIL:         builtin "assumeLessThanOrEqualTo"
// CHECK-SIL:         builtin "assumeLessThanOrEqualTo"
// CHECK-SIL:       } // end sil function '$s12range_assume17sliceOfSliceCountySiAA14TestCollectionV_SnySiGAEtF'
@inline(never)
public func sliceOfSliceCount(_ c: TestCollection, _ r: Range<Int>, _ q: Range<Int>) -> Int {
  return c[r][q].count
}

// Range's own startIndex/endIndex.
// CHECK-SIL-LABEL: sil [noinline] @$s12range_assume0A6BoundsySi_SitSnySiGF :
// CHECK-SIL:         builtin "assumeLessThanOrEqualTo"
// CHECK-SIL:       } // end sil function '$s12range_assume0A6BoundsySi_SitSnySiGF'

// CHECK-IR-LABEL: define{{.*}} @"$s12range_assume0A6BoundsySi_SitSnySiGF"
// CHECK-IR:         [[CMP:%[0-9]+]] = icmp sle i{{32|64}} %0, %1
// CHECK-IR:         call void @llvm.assume(i1 [[CMP]])
@inline(never)
public func rangeBounds(_ r: Range<Int>) -> (Int, Int) {
  return (r.startIndex, r.endIndex)
}

// The other integer types of the standard library get the assume, too.
// CHECK-SIL-LABEL: sil [noinline] @$s12range_assume16uint8RangeBoundsys5UInt8V_ADtSnyADGF :
// CHECK-SIL:         builtin "assumeLessThanOrEqualTo"
// CHECK-SIL:       } // end sil function '$s12range_assume16uint8RangeBoundsys5UInt8V_ADtSnyADGF'

// CHECK-IR-LABEL: define{{.*}} @"$s12range_assume16uint8RangeBoundsys5UInt8V_ADtSnyADGF"
// CHECK-IR:         [[CMP:%[0-9]+]] = icmp ule i8
// CHECK-IR:         call void @llvm.assume(i1 [[CMP]])
@inline(never)
public func uint8RangeBounds(_ r: Range<UInt8>) -> (UInt8, UInt8) {
  return (r.startIndex, r.endIndex)
}

// CHECK-SIL-LABEL: sil [noinline] @$s12range_assume16int32RangeBoundsys5Int32V_ADtSnyADGF :
// CHECK-SIL:         builtin "assumeLessThanOrEqualTo"
// CHECK-SIL:       } // end sil function '$s12range_assume16int32RangeBoundsys5Int32V_ADtSnyADGF'

// CHECK-IR-LABEL: define{{.*}} @"$s12range_assume16int32RangeBoundsys5Int32V_ADtSnyADGF"
// CHECK-IR:         [[CMP:%[0-9]+]] = icmp sle i32
// CHECK-IR:         call void @llvm.assume(i1 [[CMP]])
@inline(never)
public func int32RangeBounds(_ r: Range<Int32>) -> (Int32, Int32) {
  return (r.startIndex, r.endIndex)
}

public struct CustomBound: Strideable {
  public var value: Int

  @inline(never)
  public static func < (lhs: CustomBound, rhs: CustomBound) -> Bool {
    return lhs.value < rhs.value
  }

  public func distance(to other: CustomBound) -> Int { other.value - value }
  public func advanced(by n: Int) -> CustomBound { CustomBound(value: value + n) }
}

// Other bound types get no assume, and their comparison is not called.
// CHECK-SIL-LABEL: sil [noinline] @$s12range_assume17customRangeBoundsyAA11CustomBoundV_ADtSnyADGF :
// CHECK-SIL-NOT:     builtin "assumeLessThanOrEqualTo"
// CHECK-SIL-NOT:     apply
// CHECK-SIL:       } // end sil function '$s12range_assume17customRangeBoundsyAA11CustomBoundV_ADtSnyADGF'
@inline(never)
public func customRangeBounds(_ r: Range<CustomBound>) -> (CustomBound, CustomBound) {
  return (r.startIndex, r.endIndex)
}

// Without specialization the bound type is unknown: neither an assume nor the comparison is emitted.
// CHECK-IR-LABEL: define{{.*}} @"$s12range_assume18genericAssumeValidyySnyxGSLRzlF"
// CHECK-IR-NOT:     call
// CHECK-IR:         ret void
@_semantics("optimize.sil.specialize.generic.never")
@inline(never)
public func genericAssumeValid<Bound: Comparable>(_ r: Range<Bound>) {
  r._assumeValid()
}

// Iterating a range needs no checks: the assume implies both the
// `startIndex..<endIndex` precondition of `index(after:)` and that the loop's
// induction variable doesn't overflow.
// CHECK-IR-LABEL: define{{.*}} @"$s12range_assume0A3SumySiSnySiGF"
// CHECK-IR-NOT:     call void @llvm.trap()
// CHECK-IR:         {{^}}}
@inline(never)
public func rangeSum(_ r: Range<Int>) -> Int {
  var s = 0
  for i in r {
    s = s &+ i
  }
  return s
}

// The assume doesn't imply that the distance between the bounds fits in an
// Int, so `count` keeps its overflow check.
// CHECK-IR-LABEL: define{{.*}} @"$s12range_assume0A5CountySiSnySiGF"
// CHECK-IR:         call void @llvm.assume(
// CHECK-IR:         @llvm.ssub.with.overflow.i{{32|64}}(
// CHECK-IR:         call void @llvm.trap()
@inline(never)
public func rangeCount(_ r: Range<Int>) -> Int {
  return r.count
}

// A range formed by `..<` has its bounds' order checked already: the assume is
// redundant, and LLVM removes it.
// CHECK-SIL-LABEL: sil [noinline] @$s12range_assume13localRangeSumyS2iF :
// CHECK-SIL:         builtin "assumeLessThanOrEqualTo"
// CHECK-SIL:       } // end sil function '$s12range_assume13localRangeSumyS2iF'

// CHECK-IR-LABEL: define{{.*}} @"$s12range_assume13localRangeSumyS2iF"
// CHECK-IR-NOT:     @llvm.assume
// CHECK-IR:         {{^}}}
@inline(never)
public func localRangeSum(_ n: Int) -> Int {
  var s = 0
  for i in 0..<n {
    s = s &+ i
  }
  return s
}

// CHECK-SIL-LABEL: sil [noinline] @$s12range_assume20localRangeSliceCountySiSaySiG_S2itF :
// CHECK-SIL:         builtin "assumeLessThanOrEqualTo"
// CHECK-SIL:       } // end sil function '$s12range_assume20localRangeSliceCountySiSaySiG_S2itF'

// CHECK-IR-LABEL: define{{.*}} @"$s12range_assume20localRangeSliceCountySiSaySiG_S2itF"
// CHECK-IR-NOT:     @llvm.assume
// CHECK-IR:         {{^}}}
@inline(never)
public func localRangeSliceCount(_ a: [Int], _ i: Int, _ j: Int) -> Int {
  return a[i..<j].count
}

public var globalRange = 0..<10

// An assume about bounds that nothing else uses is removed along with their loads.
// CHECK-SIL-LABEL: sil [noinline] @$s12range_assume16unusedRangeBoundyyF :
// CHECK-SIL-NOT:     builtin "assumeLessThanOrEqualTo"
// CHECK-SIL-NOT:     begin_access
// CHECK-SIL-NOT:     load
// CHECK-SIL:       } // end sil function '$s12range_assume16unusedRangeBoundyyF'
@inline(never)
public func unusedRangeBound() {
  _ = globalRange.endIndex
}

// Array's range subscript, setter. The probe's body is moved into a specialized
// function; the assume must be in there.
// CHECK-SIL-LABEL: sil shared @$sSays10ArraySliceVyxGSnySiGcisSi_{{.*}} :
// CHECK-SIL:         builtin "assumeLessThanOrEqualTo"
// CHECK-SIL:       } // end sil function '$sSays10ArraySliceVyxGSnySiGcisSi_
@inline(never)
public func arraySliceSet(_ a: inout [Int], _ r: Range<Int>, _ v: ArraySlice<Int>) {
  a[r] = v
}
