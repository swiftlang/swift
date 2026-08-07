// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/use-cxx-types.swift -module-name UseCxxTy -typecheck -verify -emit-clang-header-path %t/UseCxxTy.h -I %t -cxx-interoperability-mode=default

// RUN: %FileCheck %s --input-file %t/UseCxxTy.h

// RUN: echo "#include \"header.h\"" > %t/full-cxx-swift-cxx-bridging.h
// RUN: cat %t/UseCxxTy.h >> %t/full-cxx-swift-cxx-bridging.h

// RUN: %check-interop-cxx-header-in-clang(%t/full-cxx-swift-cxx-bridging.h -Wno-reserved-identifier)

//--- header.h

struct NonCopyable {
    NonCopyable() : x(0) {}
    NonCopyable(const NonCopyable &) = delete;
    NonCopyable(NonCopyable &&other) = default;
    ~NonCopyable() = default;
    int x;
};

struct NonCopyableNonTrivial {
    NonCopyableNonTrivial() : x(0) {}
    NonCopyableNonTrivial(const NonCopyableNonTrivial &) = delete;
    NonCopyableNonTrivial(NonCopyableNonTrivial &&other) : x(other.x) { other.x = -1; }
    ~NonCopyableNonTrivial() { x = -1; }
    int x;
};

//--- module.modulemap
module CxxTest {
    header "header.h"
    requires cplusplus
}

//--- use-cxx-types.swift
import CxxTest

// CHECK: SWIFT_INLINE_THUNK NonCopyable returnNonCopyable() noexcept {{.*}}{
public func returnNonCopyable() -> NonCopyable { return NonCopyable() }

// CHECK: SWIFT_INLINE_THUNK NonCopyableNonTrivial returnNonCopyableNonTrivial() noexcept {{.*}}{
public func returnNonCopyableNonTrivial() -> NonCopyableNonTrivial { return NonCopyableNonTrivial() }

// CHECK: SWIFT_INLINE_THUNK void takeNonCopyable(const NonCopyable& x) noexcept {{.*}}{
public func takeNonCopyable(_ x: borrowing NonCopyable) {}

// CHECK: SWIFT_INLINE_THUNK void takeNonCopyableNonTrivial(const NonCopyableNonTrivial& x) noexcept {{.*}}{
public func takeNonCopyableNonTrivial(_ x: borrowing NonCopyableNonTrivial) {}

// Not yet supported.
// CHECK: Unavailable in C++: Swift global function 'consumeNonCopyable(_:)'.
public func consumeNonCopyable(_ x: consuming NonCopyable) {}
// CHECK: Unavailable in C++: Swift global function 'consumeNonCopyableNonTrivial(_:)'.
public func consumeNonCopyableNonTrivial(_ x: consuming NonCopyableNonTrivial) {}
