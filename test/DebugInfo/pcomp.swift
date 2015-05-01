// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

func markUsed<T>(t: T) {}

protocol A {
  func x()
}

protocol B {
  func y()
}

// CHECK-DAG: _TtP5pcomp1AS_1B_
func f(arg :protocol<A,B>) {
}



protocol SomeProto {
  func f() -> Int
}

class SomeClass : SomeProto {
  func f() -> Int { return 1 }
}

class SomeOtherClass : SomeClass {
  override func f() -> Int { return 1 }
}
// This is an indirect value.
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "SomeProto",{{.*}} identifier: "_TtP5pcomp9SomeProto_"
func main() {
  var p : SomeProto = SomeOtherClass()
  markUsed("\(p.f())")
}

