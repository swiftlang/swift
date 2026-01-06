// RUN: %target-swift-frontend                           \
// RUN:     -primary-file %s                             \
// RUN:     -emit-ir                                     \
// RUN:     -disable-availability-checking               \
// RUN:     -enable-builtin-module                       \
// RUN: |                                                \
// RUN: %FileCheck %s


// CHECK-LABEL: define{{.*}} ptr @"$s22bitwise_copyable_onone1EOy1AQzGAA1PRzs15BitwiseCopyableAERQlWOh"(
// CHECK-SAME:      ptr %0, 
// CHECK-SAME:      ptr %I.A, 
// CHECK-SAME:      ptr %"E<I.A>"
// CHECK-SAME:  )
enum E<Element> {
  case next(Element)
  case error(Any)
}

protocol P {
  associatedtype A
}

class C<I: P> where I.A: BitwiseCopyable {
  func takeE(_ event: E<I.A>) {}
  func run(_ e: I.A) {
    takeE(.next(e))
  }
}
