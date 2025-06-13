// RUN: %target-swift-frontend -O -enforce-exclusivity=checked -Xllvm -sil-print-types -emit-sil -Xllvm -debug-only=sil-licm -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TESTLICM
// RUN: %target-swift-frontend -O -enforce-exclusivity=checked -Xllvm -sil-print-types -emit-sil -Xllvm -debug-only=sil-licm -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TESTLICM2
// RUN: %target-swift-frontend -O -enforce-exclusivity=checked -Xllvm -sil-print-types -emit-sil  -primary-file %s | %FileCheck %s --check-prefix=TESTSIL
// RUN: %target-swift-frontend -O -enforce-exclusivity=checked -Xllvm -sil-print-types -emit-sil -Xllvm -debug-only=sil-licm -whole-module-optimization %s 2>&1 | %FileCheck %s --check-prefix=TESTLICMWMO
// RUN: %target-swift-frontend -O -enforce-exclusivity=checked -Xllvm -sil-print-types -emit-sil  -whole-module-optimization %s | %FileCheck %s --check-prefix=TESTSILWMO

// REQUIRES: optimized_stdlib,asserts,swift_stdlib_no_asserts
// REQUIRES: PTRSIZE=64

// TESTLICM-LABEL: Processing loops in {{.*}}run_ReversedArray{{.*}}
// TESTLICM: Hoist and Sink pairs attempt
// TESTLICM: Hoisted
// TESTLICM: Successfully hoisted and sank pair

// TESTSIL-LABEL: sil hidden @$s16licm_exclusivity17run_ReversedArrayyySiF : $@convention(thin) (Int) -> () {
// TESTSIL: bb
// TESTSIL: begin_access [modify] [dynamic] [no_nested_conflict]
// TESTSIL: br bb{{.*}}
// TESTSIL-EMPTY:
// TESTSIL-NEXT: {{^}}//
// TESTSIL-NEXT: {{^}}//
// TESTSIL-NEXT: bb{{.*}}:
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

// TESTLICM2-LABEL: Processing loops in {{.*}}count_unicodeScalars{{.*}}
// TESTLICM2: Hoist and Sink pairs attempt
// TESTLICM2: Hoisted

// TESTSIL-LABEL: sil @$s16licm_exclusivity20count_unicodeScalarsyySS17UnicodeScalarViewVF : $@convention(thin) (@guaranteed String.UnicodeScalarView) -> () {
// TESTSIL: bb0(%0 : $String.UnicodeScalarView)
// TESTSIL: bb5:
// TESTSIL-NEXT: [[A1:%.*]] = global_addr @$s16licm_exclusivity5countSivp : $*Int
// TESTSIL: begin_access [modify] [dynamic] [no_nested_conflict] [[A1]] : $*Int
// TESTSIL: end_access
// TESTSIL: return
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

// TESTLICMWMO-LABEL: Processing loops in {{.*}}ClassWithArrsC7readArr{{.*}}
// TESTLICMWMO: Hoist and Sink pairs attempt
// TESTLICMWMO: Hoisted
// TESTLICMWMO: Successfully hoisted and sank pair
// TESTLICMWMO: Hoisted
// TESTLICMWMO: Successfully hoisted and sank pair
// TESTSILWMO-LABEL: sil {{.*}}@$s16licm_exclusivity13ClassWithArrsC7readArryyF : $@convention(method) (@guaranteed ClassWithArrs) -> () {
// TESTSILWMO: [[R1:%.*]] = ref_element_addr %0 : $ClassWithArrs, #ClassWithArrs.A
// TESTSILWMO: [[R2:%.*]] = ref_element_addr %0 : $ClassWithArrs, #ClassWithArrs.B
// TESTSILWMO:  begin_access [read] [static] [no_nested_conflict] [[R1]]
// TESTSILWMO:  begin_access [read] [static] [no_nested_conflict] [[R2]]
  public func readArr() {
    for i in 0..<self.N {
      for j in 0..<i {
	let _ = A[j]
	let _ = B[j]
      }
    }
  }
}
