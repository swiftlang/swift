// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -enable-experimental-feature LayoutPrespecialization -typecheck -module-name LayoutPrespec -emit-module-interface-path %t/LayoutPrespec.swiftinterface %s
// RUN: %FileCheck %s < %t/LayoutPrespec.swiftinterface --check-prefix CHECK
// RUN: %target-swift-frontend -compile-module-from-interface %t/LayoutPrespec.swiftinterface -o %t/LayoutPrespec.swiftmodule
// RUN: %target-swift-frontend -enable-experimental-feature LayoutPrespecialization -compile-module-from-interface %t/LayoutPrespec.swiftinterface -o %t/LayoutPrespec.swiftmodule

// CHECK: #if compiler(>=5.3) && $LayoutPrespecialization
// CHECK-NEXT: @_specialize(exported: true, kind: full, where @_noMetadata A : _Class)
// CHECK-NEXT: public func test<A>(a: A) -> A
// CHECK-NEXT: #endif
@_specialize(exported: true, where @_noMetadata A : _Class)
public func test<A>(a: A) -> A {
    return a
}
