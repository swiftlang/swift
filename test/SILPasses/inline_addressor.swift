// RUN: %target-swift-frontend -primary-file %s -parse-as-library -Xllvm -enable-static-init=false -emit-sil -O | FileCheck %s

var inputval = 27

var totalsum = 0


// Check if the addressor functions for inputval and totalsum are
// 1) hoisted out of the loop (by GlobalOpt) and
// 2) inlined

//CHECK-LABEL: sil {{.*}}testit 
//CHECK: {{^bb0}}
//CHECK: globalinit_
//CHECK-NOT: {{^bb0}}
//CHECK: {{^bb1}}
//CHECK-NOT: globalinit
//CHECK-NOT: totalsum
//CHECK-NOT: inputval
//CHECK: {{^}$}}
func testit() {
	for i in 0...10 {
		totalsum += inputval
	}
}

