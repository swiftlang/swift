// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)

// XFAIL: *

// CHECK-LABEL: namespace Functions {

// CHECK-LABEL: namespace _impl {

// CHECK: SWIFT_EXTERN void $s9Functions16alwaysDeprecatedyyF(void) SWIFT_NOEXCEPT SWIFT_CALL SWIFT_DEPRECATED; // alwaysDeprecated()
// CHECK: SWIFT_EXTERN void $s9Functions19alwaysDeprecatedTwoyyF(void) SWIFT_NOEXCEPT SWIFT_CALL SWIFT_DEPRECATED_MSG("it should not be used"); // alwaysDeprecatedTwo()
// CHECK: SWIFT_EXTERN void $s9Functions17alwaysUnavailableyyF(void) SWIFT_NOEXCEPT SWIFT_CALL SWIFT_UNAVAILABLE; // alwaysUnavailable()
// CHECK: SWIFT_EXTERN void $s9Functions24alwaysUnavailableMessageyyF(void) SWIFT_NOEXCEPT SWIFT_CALL SWIFT_UNAVAILABLE_MSG("stuff happened"); // alwaysUnavailableMessage()
// CHECK: SWIFT_EXTERN void $s9Functions22singlePlatAvailabilityyyF(void) SWIFT_NOEXCEPT SWIFT_CALL SWIFT_AVAILABILITY(macos,introduced=11); // singlePlatAvailability()

// CHECK: }

// CHECK: inline void alwaysDeprecated() noexcept SWIFT_DEPRECATED {
@available(*, deprecated)
public func alwaysDeprecated() {}

// CHECK: inline void alwaysDeprecatedTwo() noexcept SWIFT_DEPRECATED_MSG("it should not be used")
@available(*, deprecated, message: "it should not be used")
public func alwaysDeprecatedTwo() {}

// CHECK: inline void alwaysUnavailable() noexcept SWIFT_UNAVAILABLE
@available(*, unavailable)
public func alwaysUnavailable() {}

// CHECK: inline void alwaysUnavailableMessage() noexcept SWIFT_UNAVAILABLE_MSG("stuff happened")
@available(*, unavailable, message: "stuff happened")
public func alwaysUnavailableMessage() {}

// CHECK: inline void singlePlatAvailability() noexcept SWIFT_AVAILABILITY(macos,introduced=11)
@available(macOS 11, *)
public func singlePlatAvailability() {}
