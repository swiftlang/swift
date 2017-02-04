// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

struct Foo<T> {}
struct Bar {}

extension Foo where T == Bar {
  func foo(x: T) -> Bar {
    // CHECK-LABEL: sil shared @{{.*}}3foo{{.*}}4foo2{{.*}} : $@convention(thin) (Bar) -> Bar
    func foo2() -> Bar {
      return x
    }
    return foo2()
  }
}
