// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -typecheck -module-name UseOptional -enable-experimental-cxx-interop -emit-clang-header-path %t/stdlib.h

// RUN: %FileCheck %s < %t/stdlib.h

@_expose(Cxx)
public func test() -> String {
    return ""
}

// CHECK: namespace Swift {
// CHECK: class String final {
