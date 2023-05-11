// RUN: %target-swift-frontend %use_no_opaque_pointers %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir

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

// CHECK-LABEL: define hidden swiftcc void @"$s40sil_witness_tables_inherited_conformance4feed{{[_0-9a-zA-Z]*}}F"(%T40sil_witness_tables_inherited_conformance3CatC* %0, %swift.type* %T)
func feed<T : Cat>(_ t: T) {
  // CHECK: call swiftcc void @"$s40sil_witness_tables_inherited_conformance5breed{{[_0-9a-zA-Z]*}}F"{{.*}} @"$s40sil_witness_tables_inherited_conformance3CatCAA5PandaAAWP"
  breed(t)
}

func obtain<T : Panda>(_ t: T.Type) {
  t.init()
}

// CHECK-LABEL: define hidden swiftcc void @"$s40sil_witness_tables_inherited_conformance6wangle{{[_0-9a-zA-Z]*}}F"(%swift.type* %0, %swift.type* %T)
func wangle<T : Cat>(_ t: T.Type) {
  // CHECK: call swiftcc void @"$s40sil_witness_tables_inherited_conformance6obtain{{[_0-9a-zA-Z]*}}F"{{.*}} @"$s40sil_witness_tables_inherited_conformance3CatCAA5PandaAAWP"
  obtain(t)
}

feed(Cat())
wangle(Cat.self)
Anesthesiologist<Cat>().disentangle(Cat())
