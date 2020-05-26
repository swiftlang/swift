// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

class A {}
class B: A {}

func test<T>(_ type: T.Type) -> T? {
  fatalError()
}

// CHECK: [[RESULT:%.*]] = function_ref @$s12rdar628426514testyxSgxmlF
// CHECK-NEXT: apply [[RESULT]]<B>({{.*}})
let _: A? = test(B.self)
