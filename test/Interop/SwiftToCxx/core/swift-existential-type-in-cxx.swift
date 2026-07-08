// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Core -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/core.h -enable-experimental-feature CxxExistentialInterop
// RUN: %FileCheck %s < %t/core.h

// RUN: %check-interop-cxx-header-in-clang(%t/core.h)

// REQUIRES: swift_feature_CxxExistentialInterop


// Verify that SwiftExistentialType<Tags...> out-of-line method definitions
// appear in the generated header after the ValueWitnessTable struct.
// The class template declaration is in _SwiftCxxInteroperability.h (included
// via #include).

// --- Check the #include of _SwiftCxxInteroperability.h ---
// CHECK: _SwiftCxxInteroperability.h

// --- Check the out-of-line method definitions in the scaffolding ---

// CHECK-LABEL: // Out-of-line SwiftExistentialType methods.

// CHECK: SwiftExistentialType<Tags...>::_getVWT() const noexcept {
// CHECK:   auto *vwTableAddr = reinterpret_cast<ValueWitnessTable **>(_type) - 1;
// CHECK: }

// CHECK: SwiftExistentialType<Tags...>::_initializeWithCopy(
// CHECK:     const SwiftExistentialType<Tags...> &src) noexcept {
// CHECK:   _type = src._type;
// CHECK:   _getVWT()->initializeBufferWithCopyOfBuffer(
// CHECK: }

// CHECK: SwiftExistentialType<Tags...>::_initializeWithValue(
// CHECK:     const void *_Nonnull src) noexcept {
// CHECK:   auto *vwt = _getVWT();
// CHECK:   if (!(vwt->flags & 131072)) {
// CHECK:     vwt->initializeWithCopy(
// CHECK:   } else {
// CHECK:     auto box = swift_allocBox(_type);
// CHECK:     _buffer[0] = box.object;
// CHECK:     vwt->initializeWithCopy(
// CHECK:   }
// CHECK: }

// CHECK: SwiftExistentialType<Tags...>::_projectValue() const noexcept {
// CHECK:   auto *vwTable = _getVWT();
// CHECK:   if (!(vwTable->flags & 131072))
// CHECK: }

// CHECK: SwiftExistentialType<Tags...>::SwiftExistentialType(
// CHECK:     const SwiftExistentialType<Tags...> &other) noexcept
// CHECK:   _getVWT()->initializeBufferWithCopyOfBuffer(
// CHECK: }

// CHECK: SwiftExistentialType<Tags...>::SwiftExistentialType(
// CHECK:     SwiftExistentialType<Tags...> &&other) noexcept
// CHECK:   _getVWT()->initializeBufferWithCopyOfBuffer(
// CHECK: }

// CHECK: SwiftExistentialType<Tags...>::operator=(
// CHECK:     const SwiftExistentialType<Tags...> &other) noexcept
// CHECK:     _destroyValue();
// CHECK:     _getVWT()->initializeBufferWithCopyOfBuffer(
// CHECK: }

// CHECK: SwiftExistentialType<Tags...>::operator=(
// CHECK:     SwiftExistentialType<Tags...> &&other) noexcept {
// CHECK:     _destroyValue();
// CHECK:     _getVWT()->initializeBufferWithCopyOfBuffer(
// CHECK: }

// CHECK: SwiftExistentialType<Tags...>::~SwiftExistentialType() noexcept {
// CHECK:   _destroyValue();
// CHECK: }

// --- Generic subset conversion operator (subsumes conversion to Any) ---

// CHECK: SwiftExistentialType<Tags...>::operator SwiftExistentialType<TargetTags...>() const noexcept {
// CHECK:   SwiftExistentialType<TargetTags...> result(
// CHECK:       typename SwiftExistentialType<TargetTags...>::uninit_t{});
// CHECK:   result._type = _type;
// CHECK:   _getVWT()->initializeBufferWithCopyOfBuffer(
// CHECK: }

// --- SwiftClassExistentialType generic subset conversion ---

// CHECK: SwiftClassExistentialType<Tags...>::operator SwiftExistentialType<TargetTags...>() const noexcept {
// CHECK:   SwiftExistentialType<TargetTags...> result(
// CHECK:       typename SwiftExistentialType<TargetTags...>::uninit_t{});
// CHECK:   result._type = swift_getObjectType(
// CHECK: }

// CHECK: } // namespace _impl
