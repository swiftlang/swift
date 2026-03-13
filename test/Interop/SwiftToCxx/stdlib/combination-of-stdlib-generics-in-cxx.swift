// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -module-name UseOptional -enable-experimental-cxx-interop -typecheck -verify -emit-clang-header-path %t/stdlib.h
// RUN: %FileCheck %s < %t/stdlib.h

// RUN: %check-interop-cxx-header-in-clang(-DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY %t/stdlib.h -Wno-unused-private-field -Wno-unused-function)

@_expose(Cxx)
public func testOptIntArray() -> [Int]? {
    return []
}

// CHECK: SWIFT_INLINE_THUNK swift::Optional<swift::Array<swift::Int>> testOptIntArray()
