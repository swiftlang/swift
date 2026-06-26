// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Core -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/core.h
// RUN: %FileCheck %s < %t/core.h

// RUN: %check-interop-cxx-header-in-clang(%t/core.h)

// REQUIRES: objc_interop

// Verify that SwiftExistentialType out-of-line method definitions
// appear in the generated header after the ValueWitnessTable struct.
// The class declaration is in _SwiftCxxInteroperability.h (included
// via #include).

// --- Check the #include of _SwiftCxxInteroperability.h ---
// CHECK: _SwiftCxxInteroperability.h

// --- Check the out-of-line method definitions in the scaffolding ---

// CHECK-LABEL: // Out-of-line method definitions for SwiftExistentialType.

// CHECK: SwiftExistentialType::_getVWT() const noexcept {
// CHECK:   auto *vwTableAddr = reinterpret_cast<ValueWitnessTable **>(_type) - 1;
// CHECK: }

// CHECK: SwiftExistentialType::_projectValue() const noexcept {
// CHECK:   auto *vwTable = _getVWT();
// CHECK: }

// CHECK: SwiftExistentialType::SwiftExistentialType(
// CHECK:     const SwiftExistentialType &other) noexcept
// CHECK:   _getVWT()->initializeBufferWithCopyOfBuffer(
// CHECK: }

// CHECK: SwiftExistentialType::SwiftExistentialType(
// CHECK:     SwiftExistentialType &&other) noexcept
// CHECK:   _getVWT()->initializeBufferWithCopyOfBuffer(
// CHECK: }

// CHECK: SwiftExistentialType::operator=(
// CHECK:     const SwiftExistentialType &other) noexcept {
// CHECK:     _getVWT()->destroy(
// CHECK:     _getVWT()->initializeBufferWithCopyOfBuffer(
// CHECK: }

// CHECK: SwiftExistentialType::operator=(
// CHECK:     SwiftExistentialType &&other) noexcept {
// CHECK:     _getVWT()->destroy(
// CHECK:     _getVWT()->initializeBufferWithCopyOfBuffer(
// CHECK: }

// CHECK: SwiftExistentialType::~SwiftExistentialType() noexcept {
// CHECK:   _getVWT()->destroy(
// CHECK: }

// CHECK: } // namespace _impl
