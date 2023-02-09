// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library %S/Inputs/custom-modules/IncompleteTypes/complete-swift-types.swift -module-name CompleteSwiftTypes -emit-module -emit-module-path %t/CompleteSwiftTypes.swiftmodule
// RUN: not %target-swift-frontend -enable-import-objc-forward-declarations -enable-objc-interop -typecheck -I %S/Inputs/custom-modules/IncompleteTypes -I %t %s 2>&1  | %FileCheck %s
// RUN: not %target-swift-frontend -swift-version 6 -enable-objc-interop -typecheck -I %S/Inputs/custom-modules/IncompleteTypes -I %t %s 2>&1 | %FileCheck  %s

// REQUIRES: objc_interop

import CompleteSwiftTypes
import ObjCLibraryForwardDeclaringCompleteSwiftTypes

let qux = Qux()

takeAConflictingTypeName(qux)
// CHECK:      incomplete_objc_types_swift_definition_imported_name_conflict.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'takeAConflictingTypeName' in scope
// CHECK-NEXT: takeAConflictingTypeName(qux)
// CHECK-NEXT: ^~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'takeAConflictingTypeName' not imported
// CHECK-NEXT: void takeAConflictingTypeName(ConflictingTypeName *param);
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: parameter 'param' not imported
// CHECK-NEXT: void takeAConflictingTypeName(ConflictingTypeName *param);
// CHECK-NEXT:                               ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'ConflictingTypeName' is incomplete
// CHECK-NEXT: void takeAConflictingTypeName(ConflictingTypeName *param);
// CHECK-NEXT:                               ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: incomplete interface 'ConflictingTypeName' shadows an imported @objc Swift class from module 'CompleteSwiftTypes'; import the generated Swift header for module 'CompleteSwiftTypes' in this header to realize this declaration
// CHECK-NEXT: @class ConflictingTypeName;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'ConflictingTypeName' forward declared here
// CHECK-NEXT: @class ConflictingTypeName;
// CHECK-NEXT: ^

_ = returnAConflictingTypeName()
// CHECK:      incomplete_objc_types_swift_definition_imported_name_conflict.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'returnAConflictingTypeName' in scope
// CHECK-NEXT: _ = returnAConflictingTypeName()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'returnAConflictingTypeName' not imported
// CHECK-NEXT: ConflictingTypeName *returnAConflictingTypeName();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: return type not imported
// CHECK-NEXT: ConflictingTypeName *returnAConflictingTypeName();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'ConflictingTypeName' is incomplete
// CHECK-NEXT: ConflictingTypeName *returnAConflictingTypeName();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: incomplete interface 'ConflictingTypeName' shadows an imported @objc Swift class from module 'CompleteSwiftTypes'; import the generated Swift header for module 'CompleteSwiftTypes' in this header to realize this declaration
// CHECK-NEXT: @class ConflictingTypeName;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'ConflictingTypeName' forward declared here
// CHECK-NEXT: @class ConflictingTypeName;
// CHECK-NEXT: ^

_ = returnAProtocolConflictingTypeName()
