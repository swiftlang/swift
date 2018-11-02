// RUN: %target-swift-frontend -O -enforce-exclusivity=checked -emit-sil -Xllvm -debug-only=sil-licm -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TEST1
// RUN: %target-swift-frontend -O -enforce-exclusivity=checked -emit-sil -Xllvm -debug-only=sil-licm  -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TEST2
// RUN: %target-swift-frontend -O -enforce-exclusivity=checked -emit-sil  -primary-file %s | %FileCheck %s --check-prefix=TESTSIL
// REQUIRES: optimized_stdlib,asserts

// TEST1-LABEL: Processing loops in {{.*}}run_ReversedArray{{.*}}
// TEST1: Hoist and Sink pairs attempt
// TEST1: Hoisted
// TEST1: Successfully hosited and sank pair

// TESTSIL-LABEL: sil hidden @$S16licm_exclusivity17run_ReversedArrayyySiF : $@convention(thin) (Int) -> () {
// TESTSIL: bb
// TESTSIL: begin_access [modify] [dynamic] [no_nested_conflict]
// TESTSIL: br bb{{.*}}
// TESTSIL-NEXT bb{{.*}}:
// TESTSIL: end_access
// TESTSIL: return
var x = 0
func run_ReversedArray(_ N: Int) {
  let array = Array(repeating: 1, count: 42)
  let reversedArray = array.reversed()

  // Iterate over the underlying type
  // ReversedRandomAccessCollection<Array<Int>>
  for _ in 1...N {
    for item in reversedArray {
      x = item
    }
  }
}

// TEST2-LABEL: Processing loops in {{.*}}count_unicodeScalars{{.*}}
// TEST2: Hoist and Sink pairs attempt
// TEST2: Hoisted
// TEST2: cloning
// TEST2: Successfully hosited and sank pair

// TESTSIL-LABEL: sil @$S16licm_exclusivity20count_unicodeScalarsyySS17UnicodeScalarViewVF : $@convention(thin) (@guaranteed String.UnicodeScalarView) -> () {
// TESTSIL: bb0(%0 : $String.UnicodeScalarView)
// TESTSIL-NEXT: %1 = global_addr @$S16licm_exclusivity5countSivp : $*Int
// TESTSIL: begin_access [modify] [dynamic] [no_nested_conflict] %1 : $*Int
// TESTSIL-NEXT: br bb1
// TESTSIL: end_access
// TESTSIL: return
var count: Int = 0
public func count_unicodeScalars(_ s: String.UnicodeScalarView) {
  for _ in s {
    count += 1
  }
}
