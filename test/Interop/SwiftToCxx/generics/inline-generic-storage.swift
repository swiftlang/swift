// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name InlineStorage -cxx-interoperability-mode=default \
// RUN:   -typecheck -verify -emit-clang-header-path %t/fragile.h
// RUN: %FileCheck %s --check-prefixes=COMMON,FRAGILE < %t/fragile.h
// RUN: %check-interop-cxx-header-in-clang(%t/fragile.h -Wno-reserved-identifier -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %target-swift-frontend %s -module-name InlineStorage -cxx-interoperability-mode=default \
// RUN:   -enable-library-evolution -typecheck -verify -emit-clang-header-path %t/resilient.h
// RUN: %FileCheck %s --check-prefixes=COMMON,RESILIENT < %t/resilient.h
// RUN: %check-interop-cxx-header-in-clang(%t/resilient.h -Wno-reserved-identifier -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// COMMON-LABEL: class SWIFT_SYMBOL("s:Sa") Array final {
// COMMON: alignas({{[0-9]+}}) char _storage[{{[0-9]+}}];
// COMMON-NEXT: friend class _impl::_impl_Array<T_0_0>;
// COMMON-LABEL: class SWIFT_SYMBOL("s:Sq") Optional final {
// COMMON: swift::_impl::OpaqueStorage _storage;
// COMMON-NEXT: friend class _impl::_impl_Optional<T_0_0>;

@frozen public struct Dependent<T> {
  public var value: T
  public init(_ value: T) { self.value = value }
}

// COMMON-LABEL: class {{.*}} Dependent final {
// COMMON: swift::_impl::OpaqueStorage _storage;

@frozen public struct InlineArray<T> {
  public var values: [T]
  public init(_ value: T) { values = [value] }
}

// COMMON-LABEL: class {{.*}} InlineArray final {
// COMMON: alignas({{[0-9]+}}) char _storage[{{[0-9]+}}];

@frozen public struct InlineByte<T> {
  public var value: UInt8
  public init(_ value: UInt8) { self.value = value }
}

// COMMON-LABEL: class {{.*}} InlineByte final {
// COMMON: alignas(1) char _storage[1];

@frozen public enum InlineEnum<T> {
  case none
  case value(UInt8)
}

// COMMON-LABEL: class {{.*}} InlineEnum final {
// COMMON: alignas(1) char _storage[2];

@_alignment(16)
@frozen public struct InlineLarge<T> {
  public var a, b, c, d: Int64
  public var value: UInt8
  public init(_ value: UInt8) {
    a = 1
    b = 2
    c = 3
    d = 4
    self.value = value
  }
}

// COMMON-LABEL: class {{.*}} InlineLarge final {
// COMMON: alignas(16) char _storage[33];

public struct Resilient<T> {
  public var value: UInt8
  public init(_ value: UInt8) { self.value = value }
}

// COMMON-LABEL: class {{.*}} Resilient final {
// FRAGILE: alignas(1) char _storage[1];
// RESILIENT: swift::_impl::OpaqueStorage _storage;

@frozen public struct Wrapper<T> {
  public var value: Resilient<T>
  public init(_ value: Resilient<T>) { self.value = value }
}

// COMMON-LABEL: class {{.*}} Wrapper final {
// FRAGILE: alignas(1) char _storage[1];
// RESILIENT: swift::_impl::OpaqueStorage _storage;

public func makeArray() -> [Int32] { [11, 22] }
public func passArray(_ value: [Int32]) -> [Int32] { value }
public func appendArray(_ value: inout [Int32]) { value.append(33) }
public func consumeArray(_ value: consuming [Int32]) -> Int32 {
  value.reduce(0, +)
}
public func identity<T>(_ value: T) -> T { value }
public func consume<T>(_ value: consuming T) {}

private var liveCount = 0
public final class Lifetime {
  public init() { liveCount += 1 }
  deinit { liveCount -= 1 }
}
public func getLiveCount() -> Int { liveCount }
public func makeTrackedArray() -> [Lifetime] { [Lifetime()] }
public func makeTrackedOptional() -> Lifetime? { Lifetime() }
public func makeOptionalString() -> String? { "inline optional" }
public func resetOptional<T>(_ value: inout T?) { value = nil }

@frozen public struct WordPair {
  public var a, b: Int
  public init(_ value: Int) { a = value; b = value + 1 }
}

@_alignment(16)
@frozen public struct AlignedByte {
  public var value: UInt8
  public init(_ value: UInt8) { self.value = value }
}

@frozen public struct LargePayload {
  public var a, b, c, d: Int64
  public init(_ value: Int64) { a = value; b = value; c = value; d = value }
}
