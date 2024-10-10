// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library %S/Inputs/custom-modules/IncompleteTypes/complete-swift-types.swift -module-name CompleteSwiftTypes -emit-module -emit-module-path %t/CompleteSwiftTypes.swiftmodule
// RUN: not %target-swift-frontend -enable-upcoming-feature ImportObjcForwardDeclarations -enable-objc-interop -typecheck -I %S/Inputs/custom-modules/IncompleteTypes -I %t %s -diagnostic-style llvm 2>&1  | %FileCheck %s
// RUN: not %target-swift-frontend -swift-version 6 -enable-objc-interop -typecheck -I %S/Inputs/custom-modules/IncompleteTypes -I %t %s -diagnostic-style llvm 2>&1 | %FileCheck  %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportObjcForwardDeclarations

import CompleteSwiftTypes
import ObjCLibraryForwardDeclaringCompleteSwiftTypes

let qux = Qux()

takeAConflictingTypeName(qux)
// CHECK:      incomplete_objc_types_swift_definition_imported_name_conflict.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'takeAConflictingTypeName' in scope
// CHECK-NEXT: takeAConflictingTypeName(qux)
// CHECK-NEXT: ^~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'takeAConflictingTypeName' unavailable (cannot import) 
// CHECK-NEXT: void takeAConflictingTypeName(ConflictingTypeName *param);
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: parameter 'param' unavailable (cannot import)
// CHECK-NEXT: void takeAConflictingTypeName(ConflictingTypeName *param);
// CHECK-NEXT:                               ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'ConflictingTypeName' is incomplete
// CHECK-NEXT: void takeAConflictingTypeName(ConflictingTypeName *param);
// CHECK-NEXT:                               ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'ConflictingTypeName' is incomplete and cannot be imported as a stub; its name conflicts with a class in module CompleteSwiftTypes
// CHECK-NEXT: @class ConflictingTypeName;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'ConflictingTypeName' forward declared here
// CHECK-NEXT: @class ConflictingTypeName;
// CHECK-NEXT: ^

_ = returnAConflictingTypeName()
// CHECK:      incomplete_objc_types_swift_definition_imported_name_conflict.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'returnAConflictingTypeName' in scope
// CHECK-NEXT: _ = returnAConflictingTypeName()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'returnAConflictingTypeName' unavailable (cannot import) 
// CHECK-NEXT: ConflictingTypeName *returnAConflictingTypeName();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: return type unavailable (cannot import)
// CHECK-NEXT: ConflictingTypeName *returnAConflictingTypeName();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'ConflictingTypeName' is incomplete
// CHECK-NEXT: ConflictingTypeName *returnAConflictingTypeName();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'ConflictingTypeName' is incomplete and cannot be imported as a stub; its name conflicts with a class in module CompleteSwiftTypes
// CHECK-NEXT: @class ConflictingTypeName;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'ConflictingTypeName' forward declared here
// CHECK-NEXT: @class ConflictingTypeName;
// CHECK-NEXT: ^

_ = returnAProtocolConflictingTypeName()
// CHECK:     incomplete_objc_types_swift_definition_imported_name_conflict.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'returnAProtocolConflictingTypeName' in scope
// CHECK-NEXT: _ = returnAProtocolConflictingTypeName()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'returnAProtocolConflictingTypeName' unavailable (cannot import) 
// CHECK-NEXT: id<ProtocolConflictingTypeName> returnAProtocolConflictingTypeName();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: return type unavailable (cannot import)
// CHECK-NEXT: id<ProtocolConflictingTypeName> returnAProtocolConflictingTypeName();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'ProtocolConflictingTypeName' is incomplete
// CHECK-NEXT: id<ProtocolConflictingTypeName> returnAProtocolConflictingTypeName();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'ProtocolConflictingTypeName' is incomplete and cannot be imported as a stub; its name conflicts with a protocol in module CompleteSwiftTypes
// CHECK-NEXT: @protocol ProtocolConflictingTypeName;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'ProtocolConflictingTypeName' forward declared here
// CHECK-NEXT: @protocol ProtocolConflictingTypeName;
// CHECK-NEXT: ^
