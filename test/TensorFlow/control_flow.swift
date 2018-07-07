// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -O -emit-sil %s -verify | %FileCheck %s

import TensorFlow

// The operand of cond_br is a TensorHandle<Bool>
public func testBoolCondForWhile() {
  let t = Tensor<Float>(1.0)
  var i = Tensor<Int32>(0)
  repeat {
    // expected-warning @+1{{value implicitly copied to the host}}
    let y = t + t
    print(y)
    i += 1
    // expected-warning @+1{{value implicitly copied to the host}}
  } while i != Tensor<Int32>(10)
}

// TODO: currently the loop condition comparison uses scalarized(), which
// generates send/recv, and uses Reshape. See if we can generate more efficient
// TF code in our stdlib.

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testBoolCondForWhile{{.*}}
// CHECK:    bb1(
// CHECK:      [[COND:%.*]] = builtin "__tfop_Reshape{{.*}} : $TensorHandle<Bool>
// CHECK:      cond_br [[COND]], bb2, bb3

public enum Pet {
  case bird, cat, dog, fish
}

// Enumerated all cases.
public func weighPet(pet: Pet) {
  var weight = Tensor<Float>(0.0)
  switch pet {
  case .bird: weight += 1.0
  case .cat: weight += 5.0
  case .dog: weight += 10.0
  case .fish: break // no tensor code here
  }
  // This is needed to work-around the current TF limitation where the `If` op
  // must produce some output tensors.
  // FIXME: lift this restriction.
  weight += 0.0
  _hostOp(weight)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}weighPet{{.*}}
//sil {{.*}}weighPet
//
// CHECK:      bb0:
// CHECK:        cond_br {{.*}}, bb1, bb6

// bb1 through bb4 are the 4 enum cases. bb5 is the merge block.
// Also, bb4 is the block for .fish, and has no tensor code.
// CHECK:      bb1:
// CHECK:        br bb5
// CHECK:      bb2:
// CHECK:        br bb5
// CHECK:      bb3:
// CHECK:        br bb5
// CHECK:      bb4:
// CHECK-NEXT:   br bb5
// CHECK:      bb5(
// CHECK:        return

// bb6 and bb7 are the synthesized blocks that compare against enum cases 1 and
// 2 (.cat and .dog).
// CHECK:      bb6:
// CHECK:        cond_br {{.*}}, bb2, bb7
// CHECK:      bb7:
// CHECK:        cond_br {{.*}}, bb3, bb4
// CHECK:      } // end sil function {{.*}}weighPet

public func weighPetWithDefault(pet: Pet) {
  var weight = Tensor<Float>(1.0)
  switch pet {
  case .cat: weight += 5.0
  default: weight += 3.0
  }
  weight += 0.0
  _hostOp(weight)
}

public func weighPetOnlyDefault(pet: Pet) {
  var weight = Tensor<Float>(1.0)
  switch pet {
  default: weight += 3.0
  }
  weight += 0.0
  _hostOp(weight)
}

// Straightline code -- nice!
// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}weighPetOnlyDefault{{.*}}
// CHECK:      bb0:
// CHECK-NOT:  bb1
// CHECK:        return
// CHECK-NEXT: }  // end sil function
