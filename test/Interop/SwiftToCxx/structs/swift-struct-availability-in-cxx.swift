// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Struct -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/struct.h
// RUN: %FileCheck %s < %t/struct.h

// RUN: %check-interop-cxx-header-in-clang(%t/struct.h)

@available(macOS 11, *)
public struct StructWithMacAvailability {
  var field: Int16

  init() {
    field = 0
  }
}

// CHECK: class SWIFT_AVAILABILITY(macos,introduced=11) SWIFT_SYMBOL("s:6Struct0A19WithMacAvailabilityV") StructWithMacAvailability;

// CHECK: class SWIFT_AVAILABILITY(macos,introduced=11) _impl_StructWithMacAvailability;
// CHECK: class SWIFT_AVAILABILITY(macos,introduced=11) SWIFT_SYMBOL("s:6Struct0A19WithMacAvailabilityV") StructWithMacAvailability final {
// CHECK: class SWIFT_AVAILABILITY(macos,introduced=11) _impl_StructWithMacAvailability {
// CHECK: struct SWIFT_AVAILABILITY(macos,introduced=11) TypeMetadataTrait<Struct::StructWithMacAvailability> {
// CHECK: struct SWIFT_AVAILABILITY(macos,introduced=11) implClassFor<Struct::StructWithMacAvailability>

@available(*, unavailable, message: "stuff happened")
public struct StructWithUnavailable {
  var field: Int16

  init() {
    field = 0
  }
}

// CHECK: class SWIFT_UNAVAILABLE_MSG("stuff happened") _impl_StructWithUnavailable;
// CHECK: class SWIFT_UNAVAILABLE_MSG("stuff happened") SWIFT_SYMBOL("s:6Struct0A15WithUnavailableV") StructWithUnavailable final
// CHECK: class SWIFT_UNAVAILABLE_MSG("stuff happened") _impl_StructWithUnavailable {
// CHECK: struct SWIFT_UNAVAILABLE_MSG("stuff happened") TypeMetadataTrait<Struct::StructWithUnavailable> {
// CHECK: struct SWIFT_UNAVAILABLE_MSG("stuff happened") implClassFor<Struct::StructWithUnavailable>
