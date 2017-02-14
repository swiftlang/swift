// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil %s -emit-ir | %FileCheck %s

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

// CHECK-LABEL: define hidden swiftcc void @_T040sil_witness_tables_inherited_conformance4feed{{[_0-9a-zA-Z]*}}F(%C40sil_witness_tables_inherited_conformance3Cat*, %swift.type* %T)
func feed<T : Cat>(_ t: T) {
  // CHECK: call swiftcc void @_T040sil_witness_tables_inherited_conformance5breed{{[_0-9a-zA-Z]*}}F{{.*}} @_T040sil_witness_tables_inherited_conformance3CatCAA5PandaAAWP
  breed(t)
}

func obtain<T : Panda>(_ t: T.Type) {
  t.init()
}

// CHECK-LABEL: define hidden swiftcc void @_T040sil_witness_tables_inherited_conformance6wangle{{[_0-9a-zA-Z]*}}F(%swift.type*, %swift.type* %T)
func wangle<T : Cat>(_ t: T.Type) {
  // CHECK: call swiftcc void @_T040sil_witness_tables_inherited_conformance6obtain{{[_0-9a-zA-Z]*}}F{{.*}} @_T040sil_witness_tables_inherited_conformance3CatCAA5PandaAAWP
  obtain(t)
}

feed(Cat())
wangle(Cat)
Anesthesiologist<Cat>().disentangle(Cat())
