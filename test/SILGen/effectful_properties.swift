// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen -target %target-swift-5.1-abi-triple %s -module-name accessors -swift-version 5 | %FileCheck --enable-var-scope %s

class C {
  // CHECK-DAG: sil hidden [ossa] @$s9accessors1CC16prop_asyncThrowsSivg : $@convention(method) @async (@guaranteed C) -> (Int, @error any Error) {
  var prop_asyncThrows : Int {
    get async throws { 0 }
  }
  // CHECK-DAG: sil hidden [ossa] @$s9accessors1CC10prop_asyncSivg : $@convention(method) @async (@guaranteed C) -> Int {
  var prop_async : Int {
    get async { 1 }
  }
  // CHECK-DAG: sil hidden [ossa] @$s9accessors1CC11prop_throwsSivg : $@convention(method) (@guaranteed C) -> (Int, @error any Error) {
  var prop_throws : Int {
    get throws { 2 }
  }
}

struct S {
  // CHECK-DAG: sil hidden [ossa] @$s9accessors1SVyS2icig : $@convention(method) @async (Int, S) -> Int {
  subscript(_ s : Int) -> Int {
    get async { 0 }
  }
  // CHECK-DAG: sil hidden [ossa] @$s9accessors1SVySiSdcig : $@convention(method) (Double, S) -> (Int, @error any Error) {
  subscript(_ s : Double) -> Int {
    get throws { 0 }
  }
}

enum E {
 // CHECK-DAG: sil hidden [ossa] @$s9accessors1EOyS2icig : $@convention(method) @async (Int, E) -> (Int, @error any Error) {
  subscript(_ e : Int) -> Int {
    get async throws { 0 }
  }
}

actor A {
 // CHECK-DAG: sil hidden [transparent] [ossa] @$s9accessors1AC10normalPropSivg : $@convention(method) (@sil_isolated @guaranteed A) -> Int {
  var normalProp : Int = 0
  // CHECK-DAG: sil hidden [ossa] @$s9accessors1AC12computedPropSivg : $@convention(method) (@sil_isolated @guaranteed A) -> Int {
  var computedProp : Int { get { 0 } }

  // CHECK-LABEL: sil hidden [ossa] @$s9accessors1AC9asyncPropSivg : $@convention(method) @async (@sil_isolated @guaranteed A) -> Int {
  // CHECK:       bb0([[SELF:%[0-9]+]] : @guaranteed $A):
  // CHECK:         hop_to_executor [[SELF]] : $A
  // CHECK:       } // end sil function '$s9accessors1AC9asyncPropSivg'
  var asyncProp : Int {
    get async { 0 }
  }

}

// CHECK-LABEL: sil hidden [ossa] @$s9accessors19testImplicitlyAsync1aSiAA1AC_tYaF : $@convention(thin) @async (@guaranteed A) -> Int {
// CHECK:         hop_to_executor
// CHECK:         apply {{%[0-9]+}}({{%[0-9]+}}) : $@convention(method) (@sil_isolated @guaranteed A) -> Int
// CHECK:         hop_to_executor
// CHECK:       } // end sil function '$s9accessors19testImplicitlyAsync1aSiAA1AC_tYaF'
func testImplicitlyAsync(a : A) async -> Int {
  return await a.computedProp
}


// CHECK-LABEL: sil hidden [ossa] @$s9accessors15testNormalAsync1aSiAA1AC_tYaF : $@convention(thin) @async (@guaranteed A) -> Int {
// CHECK:          apply {{%[0-9]+}}({{%[0-9]+}}) : $@convention(method) @async (@sil_isolated @guaranteed A) -> Int
// CHECK:       } // end sil function '$s9accessors15testNormalAsync1aSiAA1AC_tYaF'
func testNormalAsync(a : A) async -> Int {
  return await a.asyncProp
}
