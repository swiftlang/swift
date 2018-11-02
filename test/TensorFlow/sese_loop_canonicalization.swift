// This test file has various test cases to check that unrollng the loop body
// preserves the loop nesting.  Note that we use -Onone to preserve the
// structure of control flow for tests.
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Onone -emit-sil %s -verify | %FileCheck %s

import TensorFlow

// expected-warning @+1 {{value implicitly copied to the host}}
public func testLoopMovementFromOutside(_ breakIndex: Int32) -> Tensor<Int32> {
  var i: Int32 = 1
  var sum = Tensor<Int32>(0)
  let maxCount: Int32 = 100
  while i <= maxCount { // expected-note {{value used here}}
    sum += i
    if i == breakIndex {
      var k: Int32 = 0
      while k < i {
        sum += i
        k += 1
      }
      // expected-warning @+1 {{value implicitly copied to the host}}
      sum += 1
      break // expected-note {{value used here}}
    }
    i += 1
  }
  return sum
}

// CHECK-LABEL: --- XLA CFG Loops Before Canonicalize: {{.*}}testLoopMovementFromOutside{{.*}}
// CHECK: Loop at depth 1 containing: {{.*}}
//--- The following loop is the inner while loop, but is unnested in the CFG due to the `break`
// CHECK: Loop at depth 1 containing: {{.*}}

// CHECK-LABEL: --- XLA CFG Loops After Canonicalize: {{.*}}testLoopMovementFromOutside{{.*}}
// CHECK: Loop at depth 1 containing: {{.*}}
//--- The inner while loop gets nested into the outer while loop by SESE canonicalzation.
// CHECK:     Loop at depth 2 containing: {{.*}}

// CHECK: --- XLA CFG Canonicalize:  {{.*}}testLoopMovementFromOutside{{.*}}
// CHECK: [sequence
// CHECK:   <while Preheader: {{.*}}, Header: {{.*}}, exit: {{.*}}
// CHECK:     [sequence
// CHECK:       {condition Header: {{.*}}
// CHECK:         {condition Header: {{.*}}
// CHECK:           [sequence
// CHECK:             <while Preheader: {{.*}}, Header: {{.*}}, exit: {{.*}}
// CHECK:               [sequence
// CHECK:                 {condition Header: {{.*}}
// CHECK:                   block {{.*}}
// CHECK:                   block {{.*}}}
// CHECK:                 block {{.*}}]>
// CHECK:             block {{.*}}]
// CHECK:           block {{.*}}}
// CHECK:         block {{.*}}}
// CHECK:       block {{.*}}]>
// CHECK:   block {{.*}}]


// This example checks that the loop structure is preserved when we unroll
// the body of a loop during canonicalization.
//expected-warning @+1 {{value implicitly copied to the host}}
public func testLoopWithNestedLoopsRequiringUnrolling(
  _ breakIndex:Int32, _ repetitions: Int32) -> Tensor<Int32> {
	var result = Tensor<Int32>(0)
	for _ in 1...repetitions {
		var i = result
		// On all exits of this loop, `result` is assigned a value that is computed
		// in the loop. Therefore, we cannot avoid undefs unless we unroll the body
		// once.  This corresponds to the loop at depth 2 in the dump of loop info
		// before canonicalization.
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

// CHECK-LABEL: --- XLA CFG Loops Before Canonicalize: {{.*}}LoopWithNestedLoopsRequiringUnrolling{{.*}}
// CHECK: Loop at depth 1 containing: {{.*}}
// CHECK:     Loop at depth 2 containing: {{.*}}
// CHECK:         Loop at depth 3 containing: {{.*}}

// CHECK-LABEL: --- XLA CFG Loops After Canonicalize: {{.*}}LoopWithNestedLoopsRequiringUnrolling{{.*}}
// CHECK: Loop at depth 1 containing: {{.*}}
// CHECK:     Loop at depth 2 containing: {{.*}}
// CHECK:         Loop at depth 3 containing: {{.*}}
//--- The following loop is created as a result of unrolling body of the loop at
//--- depth 2 above and corresponds to loop at depth 3 above. Since we are
//--- nesting the inner loops into the outer loop of depth 1, the depth is 2 now.
// CHECK:     Loop at depth 2 containing: {{.*}}

//-- Check the structure of the SESE region. (Note the unrolled loop.)
// CHECK-LABEL: --- XLA CFG Canonicalize: {{.*}}LoopWithNestedLoopsRequiringUnrolling{{.*}}
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
// ---The body of this loop is unrolled above---
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

// This example checks that the loop structure is preserved when we unroll
// the body of a loop during canonicalization.
//expected-warning @+1 {{value implicitly copied to the host}}
public func testLoopWithDoublyNestedLoopsRequiringUnrolling(
	_ breakIndex:Int32, _ repetitions: Int32) -> Tensor<Int32> {
	var result = Tensor<Int32>(0)
	for _ in 1...repetitions {
		var i = result
		// On all exits of this loop, `result` is assigned a value that is computed
		// in the loop. Therefore, we cannot avoid undefs unless we unroll the body
		// once.  This corresponds to the loop at depth 2 in the dump of loop info
		// before canonicalization.
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


// CHECK-LABEL: --- XLA CFG Loops Before Canonicalize: {{.*}}LoopWithDoublyNestedLoopsRequiringUnrolling{{.*}}
// CHECK: Loop at depth 1 containing: {{.*}}
// CHECK:     Loop at depth 2 containing: {{.*}}
// CHECK:         Loop at depth 3 containing: {{.*}}
// CHECK:             Loop at depth 4 containing: {{.*}}

// CHECK-LABEL: --- XLA CFG Loops After Canonicalize: {{.*}}LoopWithDoublyNestedLoopsRequiringUnrolling{{.*}}
// CHECK: Loop at depth 1 containing: {{.*}}
// CHECK:     Loop at depth 2 containing: {{.*}}
// CHECK:         Loop at depth 3 containing: {{.*}}
// CHECK:             Loop at depth 4 containing: {{.*}}
//--- The following loop is created as a result of unrolling body of the loop at
//--- depth 2 above and corresponds to loop at depth 3 above. Since we are nesting
//--- the inner loops into the outer loop of depth 1, the depths are 2 and 3 nnow.
// CHECK:     Loop at depth 2 containing: {{.*}}
// CHECK:         Loop at depth 3 containing: {{.*}}

//-- Check the structure of the SESE region. (Note the unrolled doubly nested loop.)
// CHECK-LABEL: --- XLA CFG Canonicalize: {{.*}}LoopWithDoublyNestedLoopsRequiringUnrolling{{.*}}
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
// ---The body of this loop is unrolled above---
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
