// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -typecheck -module-name UseOptional -enable-experimental-cxx-interop -emit-clang-header-path %t/stdlib.h
// RUN: %target-swift-frontend -typecheck %s -typecheck -module-name UseOptional2 -enable-experimental-cxx-interop -emit-clang-header-path %t/stdlib2.h

// RUN: %FileCheck %s < %t/stdlib.h

// RUN: cat %t/stdlib.h %t/stdlib2.h > %t/two_includes.h

// RUN: %check-interop-cxx-header-in-clang(%t/two_includes.h -Wno-unused-private-field -Wno-unused-function -Wno-shadow)

@_expose(Cxx)
public func test() -> String {
    return ""
}

// CHECK: namespace Swift {
// CHECK: class String final {
