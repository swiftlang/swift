// RUN: %target-swift-ide-test -print-module -module-to-print=FRTConstructors -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default -print-implicit-attrs | %FileCheck %s

// A constructor marked `availability(swift, unavailable)` must propagate onto
// the synthesized foreign reference type initializer, so the imported `init`
// is `@available(*, unavailable)`.

// CHECK:      class FRTUnavailableCtor {
// CHECK-NEXT:   @available(*, unavailable, message: "cannot use this constructor")
// CHECK-NEXT:   init()

// Only the unavailable overload becomes unavailable; the available default
// initializer must import without any availability attribute.
// CHECK:      class FRTMixedAvailabilityCtors {
// CHECK-NEXT:   init(){{$}}
// CHECK-NEXT:   @available(*, unavailable, message: "cannot construct from an int")
// CHECK-NEXT:   init(_ __unnamed_param_0: CInt)
// CHECK-NEXT:   @available(*, unavailable, message: "cannot construct from two ints")
// CHECK-NEXT:   init(_ __unnamed_param_0: CInt, _ __unnamed_param_1: CInt)
// CHECK-NEXT:   @available(*, deprecated, message: "don't construct from three ints")
// CHECK-NEXT:   init(_ __unnamed_param_0: CInt, _ __unnamed_param_1: CInt, _ __unnamed_param_2: CInt)
