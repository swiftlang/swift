// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=i386_or_x86_64

// <rdar://problem/13793646>
struct OptionalStreamAdaptor<T : IteratorProtocol> {
  // CHECK: define hidden swiftcc void @_T015generic_ternary21OptionalStreamAdaptorV4next{{[_0-9a-zA-Z]*}}F(%TSq{{.*}}* noalias nocapture sret, %swift.type* %"OptionalStreamAdaptor<T>", %T15generic_ternary21OptionalStreamAdaptorV* nocapture swiftself dereferenceable({{.*}}))
  mutating
  func next() -> Optional<T.Element> {
    return x[0].next()
  }
  var x: [T]
}
