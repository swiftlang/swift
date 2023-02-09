// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library %S/Inputs/custom-modules/IncompleteTypes/complete-swift-types.swift -module-name CompleteSwiftTypes -emit-module -emit-module-path %t/CompleteSwiftTypes.swiftmodule
// RUN: not %target-swift-frontend -enable-import-objc-forward-declarations -enable-objc-interop -typecheck -I %S/Inputs/custom-modules/IncompleteTypes -I %t %s 2>&1 | tee ~/two-one.txt | %FileCheck %s
// RUN: not %target-swift-frontend -swift-version 6 -enable-objc-interop -typecheck -I %S/Inputs/custom-modules/IncompleteTypes -I %t %s 2>&1 | tee ~/two-two.txt | %FileCheck  %s

// REQUIRES: objc_interop

@_implementationOnly import CompleteSwiftTypes
import ObjCLibraryForwardDeclaringCompleteSwiftTypes

_ = returnAFoo()
// CHECK:      incomplete_objc_types_swift_definition_imported_implementationOnly.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'returnAFoo' in scope
// CHECK-NEXT: _ = returnAFoo()
// CHECK-NEXT:     ^~~~~~~~~~
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
