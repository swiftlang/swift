// This test file has various test cases to check that cloning preserves the
// loop nesting.  Note that we use -Onone to preserve the structure of control
// flow for tests.
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Onone -emit-sil %s -verify | %FileCheck %s

import TensorFlow

// This example checks that the loop structure is preserved when we clone
// the body of a loop during canonicalization.
//expected-warning @+1 {{value implicitly copied to the host}}
public func testLoopWithNestedLoopsRequiringCloning(
	_ breakIndex:Int32, _ repetitions: Int32) -> Tensor<Int32> {
	var result = Tensor<Int32>(0)
	for _ in 1...repetitions {
		var i = result
		// On all exits of this loop, `result` is assigned a value that is computed
		// in the loop. Therefore, we cannot avoid undefs unless we clone the body.
		repeat {
			// expected-warning @+1 {{value implicitly copied to the host}}
			if i > breakIndex {
				// + 1 so that this is not a simple assignment. 
				// (Simple assignments won't trigger cloning.)
				result = i + 1
				break
			}
			// Crazy way to do `k = i` so that we have a loop when we unroll the body.
			var k = Tensor<Int32>(0)
			// expected-warning @+1 {{value implicitly copied to the host}}
			while k < i {
				k += 1
			}
			i += 1
			// + k so that this is not a simple assignment. 
			// (Simple assignments won't trigger cloning.)
			result = i + k
			// expected-warning @+1 {{value implicitly copied to the host}}
		} while i <= 100
		result += 3
	}
	// expected-note @+1 {{value used here}}
	return result
}

// CHECK-LABEL: --- XLA CFG Loops Before Canonicalize: {{.*}}LoopWithNestedLoopsRequiringCloning{{.*}}
// CHECK: Loop at depth 1 containing: {{.*}}
// CHECK:     Loop at depth 2 containing: {{.*}}
// CHECK:         Loop at depth 3 containing: {{.*}}

// CHECK-LABEL: --- XLA CFG Loops After Canonicalize: {{.*}}LoopWithNestedLoopsRequiringCloning{{.*}}
// CHECK: Loop at depth 1 containing: {{.*}}
// CHECK:     Loop at depth 2 containing: {{.*}}
// CHECK:         Loop at depth 3 containing: {{.*}}
// CHECK:     Loop at depth 2 containing: {{.*}}

//-- Check the structure of the SESE region. (Note the cloned loop.)
// CHECK-LABEL: --- XLA CFG Canonicalize: {{.*}}LoopWithNestedLoopsRequiringCloning{{.*}}
// CHECK: [sequence
// CHECK:   <while Preheader: {{.*}}, Header: {{.*}}, exit: {{.*}}
// CHECK:     [sequence
// CHECK:       {condition Header: {{.*}}
// CHECK:         [sequence
// CHECK:           {condition Header: {{.*}}
// CHECK:             block {{.*}}
// CHECK:             [sequence
// CHECK:               <while Preheader: {{.*}}, Header: {{.*}}, exit: {{.*}}
// CHECK:                 [sequence
// CHECK:                   {condition Header: {{.*}}
// CHECK:                     block {{.*}}
// CHECK:                     block {{.*}}}
// CHECK:                   block {{.*}}]>
// CHECK:               {condition Header: {{.*}}
// CHECK:                 block {{.*}}
// CHECK:                 block {{.*}}}]}
// CHECK:           <while Preheader: {{.*}}, Header: {{.*}}, exit: {{.*}}
// CHECK:             [sequence
// CHECK:               {condition Header: {{.*}}
// CHECK:                 block {{.*}}
// CHECK:                 [sequence
// CHECK:                   <while Preheader: {{.*}}, Header: {{.*}}, exit: {{.*}}
// CHECK:                     [sequence
// CHECK:                       {condition Header: {{.*}}
// CHECK:                         block {{.*}}
// CHECK:                         block {{.*}}}
// CHECK:                       block {{.*}}]>
// CHECK:                   {condition Header: {{.*}}
// CHECK:                     block {{.*}}
// CHECK:                     block {{.*}}}]}
// CHECK:               block {{.*}}]>
// CHECK:           block {{.*}}]
// CHECK:         block {{.*}}}
// CHECK:       block {{.*}}]>
// CHECK:   block {{.*}}]
// CHECK: --- XLA CFG Canonicalize end

// This example checks that the loop structure is preserved when we clone
// the body of a loop during canonicalization.
//expected-warning @+1 {{value implicitly copied to the host}}
public func testLoopWithDoublyNestedLoopsRequiringCloning(
	_ breakIndex:Int32, _ repetitions: Int32) -> Tensor<Int32> {
	var result = Tensor<Int32>(0)
	for _ in 1...repetitions {
		var i = result
		// On all exits of this loop, `result` is assigned a value that is computed
		// in the loop. Therefore, we cannot avoid undefs unless we clone the body.
		repeat {
			// expected-warning @+1 {{value implicitly copied to the host}}
			if i > breakIndex {
				// + 1 so that this is not a simple assignment. 
				// (Simple assignments won't trigger cloning.)
				result = i + 1
				break
			}

			// Crazy way to do k = i^2 so that we have a doubly loop when we unroll.
			var k = Tensor<Int32>(0)
			// expected-warning @+1 {{value implicitly copied to the host}}
			while k < i {
				var w = Tensor<Int32>(0)
				// expected-warning @+1 {{value implicitly copied to the host}}
				while w < i {
					w += 1
				}
				k += 1
			}
			i += 1
			// + k so that this is not a simple assignment. 
			// (Simple assignments won't trigger cloning.)
			result = i + k
			// expected-warning @+1 {{value implicitly copied to the host}}
		} while i <= 100
		result += 3
	}
	// expected-note @+1 {{value used here}}
	return result
}


// CHECK-LABEL: --- XLA CFG Loops Before Canonicalize: {{.*}}LoopWithDoublyNestedLoopsRequiringCloning{{.*}}
// CHECK: Loop at depth 1 containing: {{.*}}
// CHECK:     Loop at depth 2 containing: {{.*}}
// CHECK:         Loop at depth 3 containing: {{.*}}
// CHECK:             Loop at depth 4 containing: {{.*}}

// CHECK: --- XLA CFG Loops Before Canonicalize end
// CHECK: Loop at depth 1 containing: {{.*}}
// CHECK:     Loop at depth 2 containing: {{.*}}
// CHECK:         Loop at depth 3 containing: {{.*}}
// CHECK:             Loop at depth 4 containing: {{.*}}
// CHECK:     Loop at depth 2 containing: {{.*}}
// CHECK:         Loop at depth 3 containing: {{.*}}

//-- Check the structure of the SESE region. (Note the cloned doubly nested loop.)
// CHECK: --- XLA CFG Canonicalize: {{.*}}LoopWithDoublyNestedLoopsRequiringCloning{{.*}}
// CHECK: [sequence
// CHECK:   <while Preheader: {{.*}}, Header: {{.*}}, exit: {{.*}}
// CHECK:     [sequence
// CHECK:       {condition Header: {{.*}}
// CHECK:         [sequence
// CHECK:           {condition Header: {{.*}}
// CHECK:             block {{.*}}
// CHECK:             [sequence
// CHECK:               <while Preheader: {{.*}}, Header: {{.*}}, exit: {{.*}}
// CHECK:                 [sequence
// CHECK:                   {condition Header: {{.*}}
// CHECK:                     [sequence
// CHECK:                       <while Preheader: {{.*}}, Header: {{.*}}, exit: {{.*}}
// CHECK:                         [sequence
// CHECK:                           {condition Header: {{.*}}
// CHECK:                             block {{.*}}
// CHECK:                             block {{.*}}}
// CHECK:                           block {{.*}}]>
// CHECK:                       block {{.*}}]
// CHECK:                     block {{.*}}}
// CHECK:                   block {{.*}}]>
// CHECK:               {condition Header: {{.*}}
// CHECK:                 block {{.*}}
// CHECK:                 block {{.*}}}]}
// CHECK:           <while Preheader: {{.*}}, Header: {{.*}}, exit: {{.*}}
// CHECK:             [sequence
// CHECK:               {condition Header: {{.*}}
// CHECK:                 block {{.*}}
// CHECK:                 [sequence
// CHECK:                   <while Preheader: {{.*}}, Header: {{.*}}, exit: {{.*}}
// CHECK:                     [sequence
// CHECK:                       {condition Header: {{.*}}
// CHECK:                         [sequence
// CHECK:                           <while Preheader: {{.*}}, Header: {{.*}}, exit: {{.*}}
// CHECK:                             [sequence
// CHECK:                               {condition Header: {{.*}}
// CHECK:                                 block {{.*}}
// CHECK:                                 block {{.*}}}
// CHECK:                               block {{.*}}]>
// CHECK:                           block {{.*}}]
// CHECK:                         block {{.*}}}
// CHECK:                       block {{.*}}]>
// CHECK:                   {condition Header: {{.*}}
// CHECK:                     block {{.*}}
// CHECK:                     block {{.*}}}]}
// CHECK:               block {{.*}}]>
// CHECK:           block {{.*}}]
// CHECK:         block {{.*}}}
// CHECK:       block {{.*}}]>
// CHECK:   block {{.*}}]
// CHECK: --- XLA CFG Canonicalize end
