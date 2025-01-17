// RUN: %target-swift-frontend -O -Xllvm -sil-print-types -emit-sil -primary-file %s | grep -v debug_value | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts
// REQUIRES: swift_in_compiler
// XFAIL: OS=linux-androideabi

// Test needs to be updated for 32bit.
// rdar://74810823
// UNSUPPORTED: PTRSIZE=32

// These tests check whether DeadObjectElimination pass runs as a part of the
// optimization pipeline and eliminates dead array literals in Swift code.
// Note that DeadObjectElimination pass relies on @_semantics annotations on
// the array initializer that is used by the compiler to create array literals.
// This test would fail if in case the initializer used by the compiler to
// initialize array literals doesn't match the one expected by the pass.

// testDeadArrayElimination requires swift_stdlib_no_asserts because,
// with runtime verification enabled, "array.finalize" becomes a
// mutating operation, preventing SILCombine from deleting it when it
// removes dead pure instructions. After inlining,
// DeadObjectElimination is still unable to remove the array because a
// second array is initialized by copying the first. This problem can be
// overcome by handling non-trivial stores in OSSA, as described here:
//   [OSSA] Improve DeadObjectElimination to handle array copies
//   https://github.com/apple/swift/issues/56179
// Once that bug is fixed, remove the requirement: swift_stdlib_no_asserts.

// CHECK-LABEL: sil hidden @$s15dead_array_elim24testDeadArrayEliminationyyF
func testDeadArrayElimination() {
  _ = [1, 2, 3]
    // CHECK: bb0:
    // CHECK-NEXT: %{{.*}} = tuple ()
    // CHECK-NEXT: return %{{.*}} : $()
}

// CHECK-LABEL: sil hidden @$s15dead_array_elim29testEmptyDeadArrayEliminationyyF
func testEmptyDeadArrayElimination() {
  _ = []
    // CHECK: bb0:
    // CHECK-NEXT: %{{.*}} = tuple ()
    // CHECK-NEXT: return %{{.*}} : $()
}

// The use case tested by the following test, where a _fixLifetime call is
// invoked on an array, appears when new os log APIs are  used.
// CHECK-LABEL: sil hidden @$s15dead_array_elim35testDeadArrayElimWithFixLifetimeUseyyF
func testDeadArrayElimWithFixLifetimeUse() {
  let a: [Int] = []
  _fixLifetime(a)
    // CHECK: bb0:
    // CHECK-NEXT: %{{.*}} = tuple ()
    // CHECK-NEXT: return %{{.*}} : $()
}

// FIXME: DeadObjectElimination doesn't optimize this yet.
func testDeadArrayElimWithAddressOnlyValues<T>(x: T, y: T) {
  _ = [x, y]
}

// Adding mark_dependence to array allocate caused this test to break
// RLE needs to handle the new init pattern - rdar://117751668
// TODO-LABEL: sil hidden {{.*}}@$s15dead_array_elim31testDeadArrayAfterOptimizationsySiSSF
// TODO:      bb0(%0 : $String):
// TODO-NEXT:   integer_literal $Builtin.Int{{[0-9]+}}, 21
// TODO-NEXT:   struct $Int
// TODO-NEXT:   return
// TODO:      } // end sil function '$s15dead_array_elim31testDeadArrayAfterOptimizationsySiSSF'
func testDeadArrayAfterOptimizations(_ stringParameter: String) -> Int {
  var sum = 0
  for x in [(1, "hello"),
            (2, "a larger string which does not fit into a small string"),
            (3, stringParameter),
            (4, "hello"),
            (5, "hello"),
            (6, "hello"),
            ] {
    sum += x.0
  }
  return sum
}

// CHECK-LABEL: sil hidden @$s15dead_array_elim15testNestedArraySiyF
// CHECK:      bb0:
// CHECK-NEXT:   integer_literal $Builtin.Int{{[0-9]+}}, 3
// CHECK-NEXT:   struct $Int
// CHECK-NEXT:   return
// CHECK:      } // end sil function '$s15dead_array_elim15testNestedArraySiyF'
func testNestedArray() -> Int {
   struct Str {
     var sa: [String]
     var s: String? = nil
     var b: Bool = true

     static func opt1(_ sa: [String], _ s: String?) -> Self {
       return .init(sa: sa, s: s, b: true)
     }

     static func opt2(_ s1: String, _ s2: String? = nil) -> Self {
       return .opt1([s1], s2)
     }

     static func opt3(_ sa: [String], _ s: String) -> Self {
       return .init(sa: sa, s: s, b: false)
     }
   }

   let strArr: [Str] = [
     .opt1(["1", "2"], "3"),
     .opt3(["4", "5"], "6"),

     .opt2("7"),
     .opt2("8"),
   ]

   var num = 0
   for s in strArr {
     if s.b {
       num += 1
     }
   }
   return num
}

