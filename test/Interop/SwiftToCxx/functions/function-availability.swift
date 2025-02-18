// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)

// CHECK-LABEL: namespace Functions SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Functions") {

// CHECK-LABEL: namespace _impl {

// CHECK: SWIFT_EXTERN void $s9Functions16alwaysDeprecatedyyF(void) SWIFT_NOEXCEPT SWIFT_CALL SWIFT_DEPRECATED; // alwaysDeprecated()
// CHECK: SWIFT_EXTERN void $s9Functions19alwaysDeprecatedTwoyyF(void) SWIFT_NOEXCEPT SWIFT_CALL SWIFT_DEPRECATED_MSG("it should not be used"); // alwaysDeprecatedTwo()
// CHECK: SWIFT_EXTERN void $s9Functions17alwaysUnavailableyyF(void) SWIFT_NOEXCEPT SWIFT_CALL SWIFT_UNAVAILABLE; // alwaysUnavailable()
// CHECK: SWIFT_EXTERN void $s9Functions24alwaysUnavailableMessageyyF(void) SWIFT_NOEXCEPT SWIFT_CALL SWIFT_UNAVAILABLE_MSG("stuff happened"); // alwaysUnavailableMessage()
// CHECK: SWIFT_EXTERN void $s9Functions22singlePlatAvailabilityyyF(void) SWIFT_NOEXCEPT SWIFT_CALL SWIFT_AVAILABILITY(macos,introduced=11); // singlePlatAvailability()

// CHECK: }

// CHECK: SWIFT_INLINE_THUNK void alwaysDeprecated() noexcept SWIFT_SYMBOL("{{.*}}") SWIFT_DEPRECATED {
@available(*, deprecated)
public func alwaysDeprecated() {}

// CHECK: SWIFT_INLINE_THUNK void alwaysDeprecatedTwo() noexcept SWIFT_SYMBOL("{{.*}}") SWIFT_DEPRECATED_MSG("it should not be used")
@available(*, deprecated, message: "it should not be used")
public func alwaysDeprecatedTwo() {}

// CHECK: SWIFT_INLINE_THUNK void alwaysUnavailable() noexcept SWIFT_SYMBOL("{{.*}}") SWIFT_UNAVAILABLE
@available(*, unavailable)
public func alwaysUnavailable() {}

// CHECK: SWIFT_INLINE_THUNK void alwaysUnavailableMessage() noexcept SWIFT_SYMBOL("{{.*}}") SWIFT_UNAVAILABLE_MSG("stuff happened")
@available(*, unavailable, message: "stuff happened")
public func alwaysUnavailableMessage() {}

// CHECK: SWIFT_INLINE_THUNK void singlePlatAvailability() noexcept SWIFT_SYMBOL("{{.*}}") SWIFT_AVAILABILITY(macos,introduced=11)
@available(macOS 11, *)
public func singlePlatAvailability() {}
