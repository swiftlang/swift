// RUN: %target-swift-emit-sil -sanitize=thread -sil-verify-all -verify -enable-experimental-feature NoImplicitCopy -enable-experimental-feature MoveOnlyClasses %s -Xllvm -sil-print-final-ossa-module | %FileCheck %s
// RUN: %target-swift-emit-sil -sanitize=thread -O -sil-verify-all -verify -enable-experimental-feature NoImplicitCopy -enable-experimental-feature MoveOnlyClasses %s
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_MoveOnlyClasses
// REQUIRES: swift_feature_NoImplicitCopy

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

        // CHECK-LABEL: sil private @$s28moveonly_addresschecker_tsan30testAssertLikeUseDifferentBitsyyF1SL_V6resume2atySi_tF : $@convention(method) (Int, @inout S) -> () {
        // CHECK-NOT: destroy_addr
        // CHECK: } // end sil function '$s28moveonly_addresschecker_tsan30testAssertLikeUseDifferentBitsyyF1SL_V6resume2atySi_tF'
        mutating func resume(at index: Int) {
            assert(index >= currentPosition)
            currentPosition = index
        }
    }
}
