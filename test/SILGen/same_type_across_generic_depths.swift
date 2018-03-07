// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership %s > %t/out.sil
// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership %t/out.sil | %FileCheck %s
class X<A> {}
struct Foo<T> {
  // CHECK-LABEL: sil hidden @{{.*}}Foo{{.*}}bar{{.*}} : $@convention(method) <T><U where T == X<U>>
  func bar<U>(_: U) where T == X<U> {}

  // CHECK-LABEL: sil hidden @{{.*}}Foo{{.*}}bar{{.*}} : $@convention(method) <T><U where T : X<U>>
  func bar<U>(_: U) where T: X<U> {}
}
