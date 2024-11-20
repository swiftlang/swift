// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift -module-name CompleteSwiftTypes -parse-as-library %S/Inputs/custom-modules/IncompleteTypes/complete-swift-types.swift -emit-module -emit-module-path %t/CompleteSwiftTypes.swiftmodule
// RUN: not %target-swift-frontend -enable-upcoming-feature ImportObjcForwardDeclarations -enable-objc-interop -typecheck -I %S/Inputs/custom-modules/IncompleteTypes -I %t %t/incomplete_definition.swift %t/full_definition.swift -diagnostic-style llvm 2>&1 | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportObjcForwardDeclarations

//--- full_definition.swift

import CompleteSwiftTypes

//--- incomplete_definition.swift

import ObjCLibraryForwardDeclaringCompleteSwiftTypes

@main
class Main {
    static func main() {
        let incompleteFoo = returnAFoo()!
        // CHECK:      incomplete_definition.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'returnAFoo' in scope
        // CHECK-NEXT: let incompleteFoo = returnAFoo()!
        // CHECK-NEXT:                     ^~~~~~~~~~
        // CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'returnAFoo' unavailable (cannot import)
        // CHECK-NEXT: Foo *returnAFoo();
        // CHECK-NEXT: ^
        // CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: return type unavailable (cannot import)
        // CHECK-NEXT: Foo *returnAFoo();
        // CHECK-NEXT: ^
        // CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Foo' is incomplete
        // CHECK-NEXT: Foo *returnAFoo();
        // CHECK-NEXT: ^
        // CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Foo' is incomplete and cannot be imported as a stub; its name conflicts with a class in module CompleteSwiftTypes
        // CHECK-NEXT: @class Foo;
        // CHECK-NEXT: ^
        // CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Foo' forward declared here
        // CHECK-NEXT: @class Foo;
        // CHECK-NEXT: ^
    }
}
