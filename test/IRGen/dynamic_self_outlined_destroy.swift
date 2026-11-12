// RUN: %target-swift-frontend -emit-ir -Onone -parse-as-library %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -O -parse-as-library %s

// The outlined destroy of S<Self> receives the metadata for S<Self>, whose
// generic argument is dynamic Self.  Binding that metadata in the outlined
// function must not try to look for fulfillments in dynamic Self, which has
// no metadata of its own.

protocol P { associatedtype A }
struct S<T: P> { var a: T.A }

class C<A>: P {
  // `is` only materializes (and then destroys) an S<Self> when the deployment
  // target predates swift_dynamicCastTest, so this doesn't exercise the
  // outlined destroy on every platform.  Just make sure it compiles.
  func f(_ x: Any) -> Bool { x is S<Self> }

  // `as?` always produces a value that has to be destroyed.
  // CHECK-LABEL: define hidden swiftcc i1 @"$s29dynamic_self_outlined_destroy1CC1gySbypF"
  // CHECK: call ptr @"$s29dynamic_self_outlined_destroy1SVyAA1CCyxGXDGSglWOh"
  func g(_ x: Any) -> Bool { (x as? S<Self>) != nil }
}

// CHECK-LABEL: define linkonce_odr hidden ptr @"$s29dynamic_self_outlined_destroy1SVyAA1CCyxGXDGSglWOh"(ptr %0, ptr %A, ptr %"S<Self>", ptr %"Optional<S<Self>>")
