// RUN: %target-swift-frontend -primary-file %s -parse-as-library -emit-sil -O | %FileCheck %s

var inputval = nonTrivialInit(false)

var totalsum = nonTrivialInit(true)


// Check if the addressor functions for inputval and totalsum are
// 1) hoisted out of the loop (by GlobalOpt) and
// 2) inlined

// CHECK-LABEL: sil hidden @$s16inline_addressor6testityySiF :
// CHECK:       {{^bb0(.*):}}
// CHECK-DAG:     function_ref @$s16inline_addressor8totalsum_WZ :
// CHECK-DAG:     builtin "once"
// CHECK-DAG:     function_ref @$s16inline_addressor8inputval_WZ :
// CHECK-DAG:     builtin "once"
// CHECK:       {{^bb1:}}
// CHECK-NOT:     "once"
// CHECK-NOT:     apply
// CHECK:       } // end sil function '$s16inline_addressor6testityySiF'
func testit(_ x: Int) {
	for _ in 0...10000000 {
		totalsum += inputval
	}
}

@inline(never)
func nonTrivialInit(_ b: Bool) -> Int {
	return b ? 0 : 27
}
