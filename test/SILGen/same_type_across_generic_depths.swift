// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-silgen -enable-sil-ownership-verifier %s > %t/out.sil
// RUN: %target-swift-emit-silgen -enable-sil-ownership-verifier %t/out.sil | %FileCheck %s
class X<A> {}
struct Foo<T> {
  // CHECK-LABEL: sil hidden @{{.*}}Foo{{.*}}bar{{.*}} : $@convention(method) <T><U where T == X<U>>
  func bar<U>(_: U) where T == X<U> {}

  // CHECK-LABEL: sil hidden @{{.*}}Foo{{.*}}bar{{.*}} : $@convention(method) <T><U where T : X<U>>
  func bar<U>(_: U) where T: X<U> {}
}
