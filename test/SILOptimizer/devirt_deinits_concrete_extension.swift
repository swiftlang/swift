// RUN: %target-swift-frontend -parse-as-library -O -sil-verify-all -module-name=test -emit-sil %s | %FileCheck %s

// REQUIRES: swift_in_compiler

// Regression test: DeinitDevirtualizer must handle ~Copyable types nested
// in constrained extensions where all generic parameters are concrete.
//
// When all generic parameters are fixed (e.g. `extension E where T == UInt8`),
// the deinit's SILFunctionType has no invocation generic signature, but
// getContextSubstitutionMap returns a non-empty map. Passing that map to
// createApply triggers an assertion:
//
//   !!subs == !!callee->getType().castTo<SILFunctionType>()
//     ->getInvocationGenericSignature()

enum Container<Element> {}

extension Container where Element == UInt8 {
  struct Storage: ~Copyable {
    var value: Int
    @inline(never) deinit {}
  }

  struct Wrapper: ~Copyable {
    var _storage: Storage
  }
}

// DestroyValueInst path: consuming parameter is destroyed at end of function.
// CHECK-LABEL: sil {{.*}}testConsumeValue
// CHECK-NOT:     destroy_value
// CHECK:         function_ref {{.*}}StorageVfD
// CHECK:         apply
// CHECK:       } // end sil function
@inline(never)
func testConsumeValue(_ s: consuming Container<UInt8>.Storage) {
}

// DestroyAddrInst path: assignment to stored property destroys old value in place.
// CHECK-LABEL: sil {{.*}}testReplaceStorage
// CHECK:         function_ref {{.*}}StorageVfD
// CHECK:         apply
// CHECK:       } // end sil function
@inline(never)
func testReplaceStorage(_ w: inout Container<UInt8>.Wrapper) {
  w._storage = .init(value: 42)
}
