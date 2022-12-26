// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -typecheck -module-name UseOptional -enable-experimental-cxx-interop -emit-clang-header-path %t/stdlib.h
// RUN: %target-swift-frontend -typecheck %s -typecheck -module-name UseOptional2 -enable-experimental-cxx-interop -emit-clang-header-path %t/stdlib2.h

// RUN: %FileCheck %s < %t/stdlib.h

// RUN: cat %t/stdlib.h %t/stdlib2.h > %t/two_includes.h

// RUN: %check-interop-cxx-header-in-clang(-DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY %t/two_includes.h -Wno-unused-private-field -Wno-unused-function -Wno-shadow)

@_expose(Cxx)
public func test() -> String {
    return ""
}

// CHECK: namespace Swift __attribute__((swift_private)) {
// CHECK: class String final {
