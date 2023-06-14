// RUN: %target-swift-frontend -parse-as-library -O -emit-sil -enable-builtin-module -module-name dap %s | %FileCheck %s

import Builtin

class TrivialDestructor {
  var int : Int
  var int2 : Int
  init() {
    self.int = 0
    self.int2 = 1
  }
  //deinit {}
}

class Kl {}

class NontrivialDestructor {
  var p : Kl 
  var i : Int 
  init() {
    self.p = Kl()
    self.i = 123
  }
}

public func makeTuple<each T>(_ t: repeat each T) -> (repeat each T) {
  (repeat each t)
}

// CHECK-LABEL: sil @$s3dap14makeEmptyTupleyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK-NEXT: [[RET:%.*]] = tuple ()
// CHECK-NEXT: return [[RET]] : $()
public func makeEmptyTuple() {
  makeTuple()
}

// CHECK-LABEL: sil @$s3dap16makeTrivialTupleyyF : $@convention(thin) () -> () {
// CHECK: bb0
// CHECK-NEXT: [[RET:%.*]] = tuple ()
// CHECK-NEXT: return [[RET]] : $()
public func makeTrivialTuple() {
  makeTuple(123, 321, 456, 654)
}

// CHECK-LABEL: sil @$s3dap26makeTrivialDestructorTupleyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK-NEXT: %0 = alloc_pack $Pack{TrivialDestructor, TrivialDestructor}
public func makeTrivialDestructorTuple() {
  makeTuple(TrivialDestructor(), TrivialDestructor())
}

// CHECK-LABEL: sil @$s3dap19makeNonTrivialTupleyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK-NEXT: %0 = alloc_pack $Pack{NontrivialDestructor, NontrivialDestructor}
public func makeNonTrivialTuple() {
  makeTuple(NontrivialDestructor(), NontrivialDestructor())
}
