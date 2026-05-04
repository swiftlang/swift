// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -enable-library-evolution %s | %FileCheck %s --check-prefix=CHECK-EVOLUTION
// RUN: %target-swift-frontend -emit-sil %s -verify
// RUN: %target-swift-frontend -emit-sil -enable-library-evolution %s -verify

public final class Klass {
  var x: Int = 0
}

public struct S {
  var _k: Klass

  public var k: Klass {
    borrow {
      return _k
    }
    mutate {
      return &_k
    }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}7testVaryyF : $@convention(thin) () -> () {
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown]
// CHECK:   [[BORROW:%.*]] = load_borrow [[ACCESS]]
// CHECK:   [[BORROW_FN:%.*]] = function_ref @$s{{.*}}1SV1k{{.*}}vb
// CHECK:   [[RESULT:%.*]] = apply [[BORROW_FN]]([[BORROW]])
// CHECK:   [[COPY:%.*]] = copy_value [[RESULT]]
// CHECK:   end_borrow [[BORROW]]
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s{{.*}}7testVaryyF'

// CHECK-EVOLUTION-LABEL: sil hidden [ossa] @$s{{.*}}7testVaryyF : $@convention(thin) () -> () {
// CHECK-EVOLUTION:   [[ACCESS:%.*]] = begin_access [read] [unknown]
// CHECK-EVOLUTION:   [[BORROW_FN:%.*]] = function_ref @$s{{.*}}1SV1k{{.*}}vb
// CHECK-EVOLUTION:   [[RESULT:%.*]] = apply [[BORROW_FN]]([[ACCESS]])
// CHECK-EVOLUTION:   [[COPY:%.*]] = copy_value [[RESULT]]
// CHECK-EVOLUTION:   end_access [[ACCESS]]
// CHECK-EVOLUTION: } // end sil function '$s{{.*}}7testVaryyF'
func testVar() {
  var s = S(_k: Klass()) // expected-warning {{variable 's' was never mutated}}
  let b = s.k
  _ = b
  _ = s
}


// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}9testInout{{.*}} : $@convention(thin) (@inout S) -> @owned Klass {
// CHECK: bb0([[ADDR:%.*]] : $*S):
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[ADDR]]
// CHECK:   [[BORROW:%.*]] = load_borrow [[ACCESS]]
// CHECK:   [[BORROW_FN:%.*]] = function_ref @$s{{.*}}1SV1k{{.*}}vb
// CHECK:   [[RESULT:%.*]] = apply [[BORROW_FN]]([[BORROW]])
// CHECK:   [[COPY:%.*]] = copy_value [[RESULT]]
// CHECK:   end_borrow [[BORROW]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   return [[COPY]]
// CHECK: } // end sil function '$s{{.*}}9testInout{{.*}}'

// CHECK-EVOLUTION-LABEL: sil hidden [ossa] @$s{{.*}}9testInout{{.*}} : $@convention(thin) (@inout S) -> @owned Klass {
// CHECK-EVOLUTION: bb0([[ADDR:%.*]] : $*S):
// CHECK-EVOLUTION:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[ADDR]]
// CHECK-EVOLUTION:   [[BORROW_FN:%.*]] = function_ref @$s{{.*}}1SV1k{{.*}}vb
// CHECK-EVOLUTION:   [[RESULT:%.*]] = apply [[BORROW_FN]]([[ACCESS]])
// CHECK-EVOLUTION:   [[COPY:%.*]] = copy_value [[RESULT]]
// CHECK-EVOLUTION:   end_access [[ACCESS]]
// CHECK-EVOLUTION:   return [[COPY]]
// CHECK-EVOLUTION: } // end sil function '$s{{.*}}9testInout{{.*}}'
func testInout(_ s: inout S) -> Klass {
  return s.k
}

