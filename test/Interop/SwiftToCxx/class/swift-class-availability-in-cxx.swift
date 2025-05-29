// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Class -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/class.h
// RUN: %FileCheck %s < %t/class.h

// RUN: %check-interop-cxx-header-in-clang(%t/class.h)

@available(macOS 11, *)
public final class ClassWithMacAvailability {
  var field: Int64

  init() {
    field = 0
  }
}

// CHECK: class SWIFT_AVAILABILITY(macos,introduced=11) _impl_ClassWithMacAvailability;
// CHECK: class SWIFT_AVAILABILITY(macos,introduced=11) SWIFT_SYMBOL("s:5Class0A19WithMacAvailabilityC") ClassWithMacAvailability final
// CHECK: class SWIFT_AVAILABILITY(macos,introduced=11) _impl_ClassWithMacAvailability {
// CHECK: struct SWIFT_AVAILABILITY(macos,introduced=11) TypeMetadataTrait<Class::ClassWithMacAvailability> {
// CHECK: struct SWIFT_AVAILABILITY(macos,introduced=11) implClassFor<Class::ClassWithMacAvailability>

@available(*, unavailable, message: "stuff happened")
public final class ClassWithUnavailable {
  var field: Int64

  init() {
    field = 0
  }
}

// CHECK: class SWIFT_UNAVAILABLE_MSG("stuff happened") _impl_ClassWithUnavailable;
// CHECK: class SWIFT_UNAVAILABLE_MSG("stuff happened") SWIFT_SYMBOL("s:5Class0A15WithUnavailableC") ClassWithUnavailable final
// CHECK: class SWIFT_UNAVAILABLE_MSG("stuff happened") _impl_ClassWithUnavailable {
// CHECK: struct SWIFT_UNAVAILABLE_MSG("stuff happened") TypeMetadataTrait<Class::ClassWithUnavailable> {
// CHECK: struct SWIFT_UNAVAILABLE_MSG("stuff happened") implClassFor<Class::ClassWithUnavailable>
