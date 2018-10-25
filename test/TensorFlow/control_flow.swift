// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -Xllvm -tf-module-level-graph=false -O -emit-sil %s -verify -enable-objc-interop -disable-objc-attr-requires-foundation-module | %FileCheck %s

import TensorFlow

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
  // This is used to avoid noisy send/recv warnings, where in each case above,
  // the computed tensor value is sent to host due to the branch inst that's not
  // marked for accelerator.
  // FIXME: Revisit sends/recvs warnings design and remove this statement.
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

// CHECK-LABEL: ---- ANALYSIS STATE FOR FUNCTION {{.*}}testCondBranch
// CHECK:       bb0:
// CHECK:       [Copy]    cond_br {{.*}}, bb1, bb2
// CHECK:       bb1:
// CHECK:       [Copy]    br bb3
// CHECK:       bb2:
// CHECK:       [Copy]    br bb3
// CHECK:       bb3:
// CHECK:           return
// CHECK-NEXT:  ---- END OF ANALYSIS STATE FOR FUNCTION

public func testCondBranch(_ a: Bool) {
  var b = Tensor<Float>(2.0)
  if a {
    b += 1.0
  }
  b -= 1.0
  _hostOp(b)
}

// For testCondBranch(), we are generating a stateless if op.
// CHECK-LABEL: --- TFPartition GraphDef Proto:
// CHECK:  op: "StatelessIf"

public func testWhile(_ n: Int32) {
  var i: Int32 = 0
  var a = Tensor<Float>(2.0)
  while i < n {
    a += 1.0
    i += 1
  }
  a += 0.0
  _hostOp(a)
}

// For testWhile(), we are generating a stateless while op.
// CHECK-LABEL: --- TFPartition GraphDef Proto:
// CHECK:  op: "StatelessWhile"

// CHECK-LABEL: ---- ANALYSIS STATE FOR FUNCTION {{.*}}testSwitchEnum
// CHECK:       bb0:
// CHECK:       [Send]    switch_enum {{.*}}, case {{.*}} bb2, case {{.*}} bb1
// CHECK:       bb1:
// CHECK-NEXT:  [Copy]    br bb3
// CHECK:       bb2:
// CHECK:       [Copy]    br bb3
// CHECK:       bb3:
// CHECK:           return
// CHECK-NEXT:  ---- END OF ANALYSIS STATE FOR FUNCTION

public func testSwitchEnum(_ a: Int?) {
  var b = Tensor<Float>(2.0)
  if let _ = a {
    b += 1.0
  }
  b -= 1.0
  _hostOp(b)
}

// CHECK-LABEL: ---- ANALYSIS STATE FOR FUNCTION {{.*}}testSwitchEnumAddr
// CHECK:       bb0:
// CHECK:       [Send]    switch_enum_addr {{.*}}, case {{.*}} bb1, case {{.*}} bb2
// CHECK:       bb1:
// CHECK:       [Copy]    br bb3
// CHECK:       bb2:
// CHECK:       [Copy]    br bb3
// CHECK:       bb3:
// CHECK:           return
// CHECK-NEXT:  ---- END OF ANALYSIS STATE FOR FUNCTION

public protocol P {}
public struct S: P {}
public enum EnumAddr {
  case A
  case B(P)
}
public func testSwitchEnumAddr(_ a: EnumAddr) {
  var b = Tensor<Float>(2.0)
  switch a {
  case .A:
      b += 1.0
  default:
      break
  }
  b -= 1.0
  _hostOp(b)
}

// CHECK-LABEL: ---- ANALYSIS STATE FOR FUNCTION {{.*}}testTryApply
// CHECK:       bb0:
// CHECK:       [Send]    try_apply {{.*}}, normal bb1, error bb3
// CHECK:       bb1:
// CHECK:       [Copy]    br bb2
// CHECK:       bb2:
// CHECK:           return
// CHECK:       bb3:
// CHECK:       [Copy]    br bb2
// CHECK-NEXT:  ---- END OF ANALYSIS STATE FOR FUNCTION

public func testTryApply(_ a: Int) {
  enum MyError : Error {
    case E(String)
  }
  @inline(never)
  func foo(_ x: Int) throws {
    if x == 1 {
      throw MyError.E("err")
    }
  }

  var b = Tensor<Float>(2.0)
  do {
    try foo(a)
    b += 1.0
  } catch { }
  b -= 1.0
  _hostOp(b)
}

// CHECK-LABEL: ---- ANALYSIS STATE FOR FUNCTION {{.*}}testCheckedCastBranch
// CHECK:       bb0:
// CHECK:       [Send]    checked_cast_br {{.*}}, bb1, bb2
// CHECK:       bb1:
// CHECK:       [Copy]    br bb3
// CHECK:       bb2:
// CHECK:       [Copy]    br bb3
// CHECK:       bb3:
// CHECK:           return
// CHECK-NEXT:  ---- END OF ANALYSIS STATE FOR FUNCTION

class X {}
class Y {}
public func testCheckedCastBranch(_ a: AnyObject) {
  var b = Tensor<Float>(2.0)
  if a is X {
    b += 1.0
  }
  b -= 1.0
  _hostOp(b)
}

// CHECK-LABEL: ---- ANALYSIS STATE FOR FUNCTION {{.*}}testCheckedCastAddrBranch
// CHECK:       bb0:
// CHECK:       [Send]    checked_cast_addr_br {{.*}}, bb1, bb2
// CHECK:       bb1:
// CHECK:       [Copy]    br bb3
// CHECK:       bb2:
// CHECK:       [Copy]    br bb3
// CHECK:       bb3:
// CHECK:           return
// CHECK-NEXT:  ---- END OF ANALYSIS STATE FOR FUNCTION

struct SS : P {}
public func testCheckedCastAddrBranch(_ p: P) {
  var b = Tensor<Float>(2.0)
  if let _ = p as? S {
    b += 1.0
  }
  b -= 1.0
  _hostOp(b)
}

// CHECK-LABEL: ---- ANALYSIS STATE FOR FUNCTION {{.*}}testDynamicMethodBranch
// CHECK:       bb0:
// CHECK:       [Send]    dynamic_method_br {{.*}}, bb1, bb2
// CHECK:       bb1:
// CHECK:       [Copy]    br bb3
// CHECK:       bb2:
// CHECK:       [Copy]    br bb3
// CHECK:       bb3:
// CHECK:           return
// CHECK-NEXT:  ---- END OF ANALYSIS STATE FOR FUNCTION

class DynamicMethodClass {
  @objc func f() {}
}
public func testDynamicMethodBranch(_ obj: AnyObject) {
  var b = Tensor<Float>(2.0)
  if let foo = obj.f {
    foo()
    b += 1.0
  }
  b -= 1.0
  _hostOp(b)
}

//===-------------------------------------------------------------------===//
// TODO: enable the following tests when relevant compiler crashes are fixed
//===-------------------------------------------------------------------===//
/*
// This generates switch_value, but it requires -Onone that crashes compiler.
@_optimize(none)
public func test(_ x: Bool) {
  var b = Tensor<Float>(2.0)
  switch (x) {
    case true: b += 1.0
    default: break
  }
  b -= 1.0
  _hostOp(b)
}

// This generates checked_cast_value_br, but it requires -enable-sil-opaque-values that crashes compiler.
public func foo<T>(_ a: T) {
  var b = Tensor<Float>(2.0)
  if let _ : T = 42 as? T {
    b += 1.0
  }
  b -= 1.0
  _hostOp(b)
}
*/

// TODO: enable this for-loop test once we resolve
// https://bugs.swift.org/browse/SR-7765:
/// SESE FIXME: Imperfect loop exits not handled yet!
// public func foo(n: Int32) {
//   var a = Tensor<Float>(1.0)
//   for _ in 0..<n {
//     a += a
//   }
//   _hostOp(a)
// }

// An infinite loop that we reject in partitioning.
// expected-error @+1 {{Functions containing infinite loops are not supported by TensorFlow yet}}
public func infLoop1() {
  let maxCount: Int32 = 100
  var a = Tensor<Int32>(0)
  let count: Int32 = 0
  while count < maxCount {
    a += a
  }
  a -= a
  _hostOp(a)
}

// TODO: Enable this test when we fix
// SESE FIXME: Imperfect loop exits not handled yet!
// public func SR8256(_ cond: Int?) {
//   let a = Tensor<Float>(1.0)
//   var i: Int32 = 0
//   let maxCount: Int32 = 10
//   while i < maxCount {
//     if cond != nil {
//       _hostOp(a+a)
//     }
//     i += 1
//   }
// }


// SR-8373: Critical edges should be split.
public func testCriticalEdges() {
  _ = Tensor(1).scalars[0..<5 * Int(2)]
  for _ in 1...5 {
    Tensor(1).scalars.forEach { _ in }
  }
}


@inline(never)
public func SR8443(n: Int32) {
  var i: Int32 = 0
  while i < n {
    // expected-warning @+1 {{implicitly copied to the host}}
    let images = Tensor<Float>(0.0)
    _hostOp(images)
    i += 1
  }

  let x = Tensor<Float>(1.0)
  _hostOp(x)
}

// Check that we have wired up control dependency before returning the result
// tensor `x`.

// CHECK-LABEL: --- TFPartition GraphDef Proto:
// CHECK:   function {
// CHECK:    signature {
// CHECK:      name: "{{.*}}SR8443{{.*}}.tf_CPU.device_partition"
// CHECK:    node_def {
// CHECK:      name: "RunControlDependency"
// CHECK:      op: "Identity"
// CHECK:      input:
// CHECK:      input: "^
// CHECK:    ret {
// CHECK:      key: "runcontroldependency"
