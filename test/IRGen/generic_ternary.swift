// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=i386_or_x86_64

// <rdar://problem/13793646>
struct OptionalStreamAdaptor<T : IteratorProtocol> {
  // CHECK: define hidden swiftcc void @_T015generic_ternary21OptionalStreamAdaptorV4next{{[_0-9a-zA-Z]*}}F(%Sq{{.*}}* noalias nocapture sret, %swift.type* %"OptionalStreamAdaptor<T>", %V15generic_ternary21OptionalStreamAdaptor* nocapture swiftself dereferenceable({{.*}}))
  mutating
  func next() -> Optional<T.Element> {
    return x[0].next()
  }
  var x: [T]
}
