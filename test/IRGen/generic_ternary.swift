// RUN: %target-swift-frontend -primary-file %s -emit-ir | FileCheck %s

// REQUIRES: CPU=i386_or_x86_64

// <rdar://problem/13793646>
struct OptionalStreamAdaptor<T: GeneratorType> {
  // CHECK: define hidden void @_TFV15generic_ternary21OptionalStreamAdaptor4nextuR_s13GeneratorTyperfRGS0_q__FT_GSqw_7Element_(%Sq{{.*}}* noalias nocapture sret, %swift.type* %Self, %V15generic_ternary21OptionalStreamAdaptor* nocapture dereferenceable({{.*}}))
  mutating
  func next() -> Optional<T.Element> {
    return x[0].next()
  }
  var x: [T]
}
