// RUN: %target-swift-frontend %s -emit-ir | FileCheck %s

// rdar://problem/20628295

protocol Panda : class {
  init()
  func getCutenessLevel() -> Int
}

class Cat : Panda {
  required init() { }

  func getCutenessLevel() -> Int {
    return 3;
  }
}

class Veterinarian<T: Panda> {
  func disentangle(t: T) { }
}

class Anasthesiologist<T: Cat> : Veterinarian<T> { }

func breed<T : Panda>(t: T) { }

// CHECK-LABEL: define hidden void @_TF40sil_witness_tables_inherited_conformance4feeduR_CS_3CatrFq_T_(%C40sil_witness_tables_inherited_conformance3Cat*, %swift.type* %T)
func feed<T : Cat>(t: T) {
  // CHECK: call void @_TF40sil_witness_tables_inherited_conformance5breeduR_S_5PandarFq_T_{{.*}}@_TWPC40sil_witness_tables_inherited_conformance3CatS_5PandaS_
  breed(t)
}

func obtain<T : Panda>(t: T.Type) {
  t.init()
}

// CHECK-LABEL: define hidden void @_TF40sil_witness_tables_inherited_conformance6wangleuR_CS_3CatrFMq_T_(%swift.type*, %swift.type* %T)
func wangle<T : Cat>(t: T.Type) {
  // CHECK: call void @_TF40sil_witness_tables_inherited_conformance6obtainuR_S_5PandarFMq_T_{{.*}}@_TWPC40sil_witness_tables_inherited_conformance3CatS_5PandaS_
  obtain(t)
}

feed(Cat())
wangle(Cat)
Anasthesiologist<Cat>().disentangle(Cat())
