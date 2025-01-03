// RUN: %target-swift-emit-sil -sil-verify-all -verify \
// RUN: -enable-experimental-feature NoImplicitCopy \
// RUN: -enable-experimental-feature MoveOnlyClasses \
// RUN: -enable-experimental-feature LifetimeDependence \
// RUN: -Xllvm -sil-print-final-ossa-module %s | %FileCheck %s

// RUN: %target-swift-emit-sil -O -sil-verify-all -verify \
// RUN: -enable-experimental-feature NoImplicitCopy \
// RUN: -enable-experimental-feature MoveOnlyClasses \
// RUN: -enable-experimental-feature LifetimeDependence \
// RUN: %s

// REQUIRES: swift_feature_MoveOnlyClasses
// REQUIRES: swift_feature_NoImplicitCopy
// REQUIRES: swift_feature_LifetimeDependence

// This file contains tests that used to crash due to verifier errors. It must
// be separate from moveonly_addresschecker_diagnostics since when we fail on
// the diagnostics in that file, we do not actually run the verifier.

struct TestTrivialReturnValue : ~Copyable {
    var i: Int = 5

    // We used to error on return buffer.
    consuming func drain() -> Int {
        let buffer = (consume self).i
        self = .init(i: 5)
        return buffer
    }
}


//////////////////////
// MARK: Misc Tests //
//////////////////////

func testAssertLikeUseDifferentBits() {
    struct S : ~Copyable {
        var s: [Int] = []
        var currentPosition = 5

        // CHECK-LABEL: sil private @$s23moveonly_addresschecker30testAssertLikeUseDifferentBitsyyF1SL_V6resume2atySi_tF : $@convention(method) (Int, @inout S) -> () {
        // CHECK-NOT: destroy_addr
        // CHECK: } // end sil function '$s23moveonly_addresschecker30testAssertLikeUseDifferentBitsyyF1SL_V6resume2atySi_tF'
        mutating func resume(at index: Int) {
            assert(index >= currentPosition)
            currentPosition = index
        }
    }
}

// issue #75312
struct S
{
    @usableFromInline
    init(utf8:consuming [UInt8])
    {
        utf8.withUnsafeBufferPointer { _ in }
        fatalError()
    }
}

struct TestCoroAccessorOfCoroAccessor<T : ~Escapable> : ~Copyable & ~Escapable {
  var t: T

  var inner: TestCoroAccessorOfCoroAccessor<T> {
    _read {
      fatalError()
    }
  }
  var outer: TestCoroAccessorOfCoroAccessor<T> {
    _read {
      yield inner
    }
  }
}
