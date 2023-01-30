// RUN: %target-swift-frontend -emit-sil -verify -module-name M %s | %FileCheck %s

// https://github.com/apple/swift/issues/52072

class BaseClass {}
class SubClass: BaseClass {}
struct Box<T> { init(_: T.Type) {} }


func test1<T>(box: Box<T>) -> T.Type {
  return T.self
}

func test2<T: BaseClass>(box: Box<T>) -> T.Type {
  return T.self
}

// CHECK: [[F1:%.*]] = function_ref @$s1M3BoxVyACyxGxmcfC
// CHECK-NEXT: apply [[F1]]<SubClass>({{.*}}, {{.*}})
_ = test1(box: .init(SubClass.self))

// CHECK: [[F2:%.*]] = function_ref @$s1M5test23boxxmAA3BoxVyxG_tAA9BaseClassCRbzlF
// CHECK-NEXT: apply [[F2]]<SubClass>({{.*}})
_ = test2(box: .init(SubClass.self))
