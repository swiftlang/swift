// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

func markUsed<T>(_ t: T) {}

protocol A {
  func x()
}

protocol B {
  func y()
}

// CHECK-DAG: _T05pcomp1A_AA1Bp
func f(_ arg : A & B) {
}



protocol SomeProto {
  func f() -> Int64
}

class SomeClass : SomeProto {
  func f() -> Int64 { return 1 }
}

class SomeOtherClass : SomeClass {
  override func f() -> Int64 { return 1 }
}
// This is an indirect value.
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "SomeProto",{{.*}} identifier: "_T05pcomp9SomeProto_pD"
func main() {
  var p : SomeProto = SomeOtherClass()
  markUsed("\(p.f())")
}

