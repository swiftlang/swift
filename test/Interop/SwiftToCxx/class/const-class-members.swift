// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name ConstMembers -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/legacy.h
// RUN: %FileCheck %s --check-prefixes=CHECK,LEGACY < %t/legacy.h
// RUN: %check-interop-cxx-header-in-clang(%t/legacy.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %target-swift-frontend %s -module-name ConstMembers -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/const.h -enable-experimental-feature GenerateConstClassMembersInCXX
// RUN: %FileCheck %s --check-prefixes=CHECK,CONST < %t/const.h
// RUN: %check-interop-cxx-header-in-clang(%t/const.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %target-swift-frontend %s -module-name ConstMembers -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/resilient.h -enable-library-evolution -enable-experimental-feature GenerateConstClassMembersInCXX
// RUN: %FileCheck %s --check-prefixes=CHECK,CONST < %t/resilient.h
// RUN: %check-interop-cxx-header-in-clang(%t/resilient.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %target-swift-frontend %s -module-name ConstMembers -emit-module -emit-module-path %t/ConstMembers.swiftmodule
// RUN: %target-swift-frontend -parse-as-library %t/ConstMembers.swiftmodule -module-name ConstMembers -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/module.h -enable-experimental-feature GenerateConstClassMembersInCXX
// RUN: %FileCheck %s --check-prefixes=CHECK,CONST < %t/module.h
// RUN: %check-interop-cxx-header-in-clang(%t/module.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// REQUIRES: swift_feature_GenerateConstClassMembersInCXX

public class Counter {
  public var value: Int
  public init(_ value: Int) { self.value = value }
  deinit { print("destroy Counter") }

  public func read() -> Int { value }
  public borrowing func increment() -> Int {
    value += 1
    return value
  }
  public consuming func consumeAndRead() -> Int { value }
  public final borrowing func finalRead() -> Int { value }
  public static func answer() -> Int { 42 }
  public static var sharedValue: Int = 0

  public var computed: Int {
    get { value }
    set { value = newValue }
  }
  public subscript(_ index: Int) -> Int { value + index }
}

public final class DerivedCounter: Counter {
  private var increments: Int = 0
  public override init(_ value: Int) { super.init(value) }
  public override func read() -> Int { value * 2 }
  public override borrowing func increment() -> Int {
    increments += 2
    return increments
  }
  public override consuming func consumeAndRead() -> Int { value * 2 }
  public override var computed: Int {
    get { value * 2 }
    set { value = newValue / 2 }
  }
}

// Keep the existing value-type convention, including mutating methods.
public struct ValueCounter {
  public var value: Int
  public init(_ value: Int) { self.value = value }
  public borrowing func read() -> Int { value }
  public mutating func increment() { value += 1 }
}

// CHECK-LABEL: class SWIFT_SYMBOL({{.*}}) Counter :
// LEGACY: swift::Int getValue() noexcept
// LEGACY: void setValue(swift::Int value) noexcept
// CONST: swift::Int getValue() const noexcept
// CONST: void setValue(swift::Int value) const noexcept
// CHECK: static SWIFT_INLINE_THUNK Counter init(swift::Int value) noexcept
// LEGACY: swift::Int read() noexcept
// LEGACY: swift::Int increment() noexcept
// LEGACY: swift::Int consumeAndRead() noexcept
// LEGACY: swift::Int finalRead() noexcept
// CONST: swift::Int read() const noexcept
// CONST: swift::Int increment() const noexcept
// CONST: swift::Int consumeAndRead() const noexcept
// CONST: swift::Int finalRead() const noexcept
// CHECK: static SWIFT_INLINE_THUNK swift::Int answer() noexcept
// CHECK: static SWIFT_INLINE_THUNK swift::Int getSharedValue() noexcept
// CHECK: static SWIFT_INLINE_THUNK void setSharedValue(swift::Int value) noexcept
// LEGACY: swift::Int getComputed() noexcept
// LEGACY: void setComputed(swift::Int newValue) noexcept
// CONST: swift::Int getComputed() const noexcept
// CONST: void setComputed(swift::Int newValue) const noexcept
// CHECK: swift::Int operator [](swift::Int index) const noexcept

// CHECK-LABEL: class SWIFT_SYMBOL({{.*}}) ValueCounter final
// CHECK: swift::Int getValue() const noexcept
// CHECK: void setValue(swift::Int value) noexcept
// CHECK: swift::Int read() const noexcept
// CHECK: void increment() noexcept

// LEGACY: Counter::consumeAndRead() noexcept {
// CONST: Counter::consumeAndRead() const noexcept {
// CHECK: auto &consumedParamCopy_this = *(new(copyBuffer_consumedParamCopy_this) Counter(*this));
// CHECK: getOpaquePointer(consumedParamCopy_this)
// LEGACY: Counter::finalRead() noexcept {
// CONST: Counter::finalRead() const noexcept {
// CHECK-NEXT: return ConstMembers::_impl::{{.*}}(::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this));
// CHECK-NEXT: }
