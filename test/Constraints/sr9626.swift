// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

class BaseClass {}
class SubClass: BaseClass {}
struct Box<T> { init(_: T.Type) {} }


func test1<T>(box: Box<T>) -> T.Type {
  return T.self
}

func test2<T: BaseClass>(box: Box<T>) -> T.Type {
  return T.self
}

// CHECK: [[F1:%.*]] = function_ref @$s6sr96263BoxVyACyxGxmcfC
// CHECK-NEXT: apply [[F1]]<SubClass>({{.*}}, {{.*}})
_ = test1(box: .init(SubClass.self))

// CHECK: [[F2:%.*]] = function_ref @$s6sr96265test23boxxmAA3BoxVyxG_tAA9BaseClassCRbzlF
// CHECK-NEXT: apply [[F2]]<SubClass>({{.*}})
_ = test2(box: .init(SubClass.self))
