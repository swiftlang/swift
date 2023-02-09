// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift -module-name CompleteSwiftTypes -parse-as-library %S/Inputs/custom-modules/IncompleteTypes/complete-swift-types.swift -emit-module -emit-module-path %t/CompleteSwiftTypes.swiftmodule
// RUN: not %target-swift-frontend -enable-import-objc-forward-declarations -enable-objc-interop -typecheck -I %S/Inputs/custom-modules/IncompleteTypes -I %t %t/incomplete_definition.swift %t/full_definition.swift 2>&1 | %FileCheck %s

// REQUIRES: objc_interop

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
        // CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'returnAFoo' not imported
        // CHECK-NEXT: Foo *returnAFoo();
        // CHECK-NEXT: ^
        // CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: return type not imported
        // CHECK-NEXT: Foo *returnAFoo();
        // CHECK-NEXT: ^
        // CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Foo' is incomplete
        // CHECK-NEXT: Foo *returnAFoo();
        // CHECK-NEXT: ^
        // CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: incomplete interface 'Foo' shadows an imported @objc Swift class from module 'CompleteSwiftTypes'; import the generated Swift header for module 'CompleteSwiftTypes' in this header to realize this declaration
        // CHECK-NEXT: @class Foo;
        // CHECK-NEXT: ^
        // CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Foo' forward declared here
        // CHECK-NEXT: @class Foo;
        // CHECK-NEXT: ^
    }
}
