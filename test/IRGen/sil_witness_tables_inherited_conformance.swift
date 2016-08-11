// RUN: %target-swift-frontend %s -emit-ir | %FileCheck %s

// rdar://problem/20628295

protocol Panda : class {
  init()
  func getCutenessLevel() -> Int
}

class Cat : Panda {
  required init() { }

  func getCutenessLevel() -> Int {
    return 3
  }
}

class Veterinarian<T: Panda> {
  func disentangle(_ t: T) { }
}

class Anesthesiologist<T: Cat> : Veterinarian<T> { }

func breed<T : Panda>(_ t: T) { }

// CHECK-LABEL: define hidden void @_TF40sil_witness_tables_inherited_conformance4feed{{.*}}(%C40sil_witness_tables_inherited_conformance3Cat*, %swift.type* %T)
func feed<T : Cat>(_ t: T) {
  // CHECK: call void @_TF40sil_witness_tables_inherited_conformance5breed{{.*}}@_TWPC40sil_witness_tables_inherited_conformance3CatS_5PandaS_
  breed(t)
}

func obtain<T : Panda>(_ t: T.Type) {
  t.init()
}

// CHECK-LABEL: define hidden void @_TF40sil_witness_tables_inherited_conformance6wangle{{.*}}(%swift.type*, %swift.type* %T)
func wangle<T : Cat>(_ t: T.Type) {
  // CHECK: call void @_TF40sil_witness_tables_inherited_conformance6obtain{{.*}}@_TWPC40sil_witness_tables_inherited_conformance3CatS_5PandaS_
  obtain(t)
}

feed(Cat())
wangle(Cat)
Anesthesiologist<Cat>().disentangle(Cat())
