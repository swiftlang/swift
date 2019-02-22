// RUN: %target-swift-frontend -O -enforce-exclusivity=checked -emit-sil -Xllvm -debug-only=sil-licm -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TEST1
// RUN: %target-swift-frontend -O -enforce-exclusivity=checked -emit-sil -Xllvm -debug-only=sil-licm  -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TEST2
// RUN: %target-swift-frontend -O -enforce-exclusivity=checked -emit-sil  -primary-file %s | %FileCheck %s --check-prefix=TESTSIL
// RUN: %target-swift-frontend -O -enforce-exclusivity=checked -emit-sil -Xllvm -debug-only=sil-licm  -whole-module-optimization %s 2>&1 | %FileCheck %s --check-prefix=TEST3
// RUN: %target-swift-frontend -O -enforce-exclusivity=checked -emit-sil  -whole-module-optimization %s | %FileCheck %s --check-prefix=TESTSIL2

// REQUIRES: optimized_stdlib,asserts

// TEST1-LABEL: Processing loops in {{.*}}run_ReversedArray{{.*}}
// TEST1: Hoist and Sink pairs attempt
// TEST1: Hoisted
// TEST1: Successfully hosited and sank pair

// TESTSIL-LABEL: sil hidden @$s16licm_exclusivity17run_ReversedArrayyySiF : $@convention(thin) (Int) -> () {
// TESTSIL: bb
// TESTSIL: begin_access [modify] [dynamic] [no_nested_conflict]
// TESTSIL: br bb{{.*}}
// TESTSIL-NEXT bb{{.*}}:
// TESTSIL: end_access
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

// FIXME: <rdar://problem/45931225> Re-enable the below
//
// xTESTSIL-LABEL: sil @$s16licm_exclusivity20count_unicodeScalarsyySS17UnicodeScalarViewVF : $@convention(thin) (@guaranteed String.UnicodeScalarView) -> () {
// xTESTSIL: bb0(%0 : $String.UnicodeScalarView)
// xTESTSIL-NEXT: %1 = global_addr @$s16licm_exclusivity5countSivp : $*Int
// xTESTSIL: begin_access [modify] [dynamic] [no_nested_conflict] %1 : $*Int
// xTESTSIL: end_access
// xTESTSIL: return
var count: Int = 0
public func count_unicodeScalars(_ s: String.UnicodeScalarView) {
  for _ in s {
    count += 1
  }
}


public class ClassWithArrs {
    var N: Int = 0
    var A: [Int]
    var B: [Int]

    init(N: Int) {
        self.N = N

        A = [Int](repeating: 0, count: N)
        B = [Int](repeating: 0, count: N)
    }

// TEST3-LABEL: Processing loops in {{.*}}ClassWithArrsC7readArr{{.*}}
// TEST3: Hoist and Sink pairs attempt
// TEST3: Hoisted
// TEST3: Successfully hosited and sank pair
// TEST3: Hoisted
// TEST3: Successfully hosited and sank pair
// TESTSIL2-LABEL: sil @$s16licm_exclusivity13ClassWithArrsC7readArryyF : $@convention(method) (@guaranteed ClassWithArrs) -> () {
// TESTSIL2: [[R1:%.*]] = ref_element_addr %0 : $ClassWithArrs, #ClassWithArrs.A
// TESTSIL2: [[R2:%.*]] = ref_element_addr %0 : $ClassWithArrs, #ClassWithArrs.B
// TESTSIL2:  begin_access [read] [static] [no_nested_conflict] [[R1]]
// TESTSIL2:  begin_access [read] [static] [no_nested_conflict] [[R2]]
    public func readArr() {
        for i in 0..<self.N {
            for j in 0..<i {
				let _ = A[j]
				let _ = B[j]
			}
        }
    }
}
