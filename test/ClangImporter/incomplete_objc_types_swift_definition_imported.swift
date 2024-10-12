// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library %S/Inputs/custom-modules/IncompleteTypes/complete-swift-types.swift -module-name CompleteSwiftTypes -emit-module -emit-module-path %t/CompleteSwiftTypes.swiftmodule
// RUN: not %target-swift-frontend -enable-upcoming-feature ImportObjcForwardDeclarations -enable-objc-interop -typecheck -I %S/Inputs/custom-modules/IncompleteTypes -I %t %s -diagnostic-style llvm 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -swift-version 6 -enable-objc-interop -typecheck -I %S/Inputs/custom-modules/IncompleteTypes -I %t %s -diagnostic-style llvm 2>&1 | %FileCheck  %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportObjcForwardDeclarations

import CompleteSwiftTypes
import ObjCLibraryForwardDeclaringCompleteSwiftTypes

let foo = Foo()
let bar = Bar()
let corge = Corge()

takeAFoo(foo)
// CHECK:      incomplete_objc_types_swift_definition_imported.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'takeAFoo' in scope
// CHECK-NEXT: takeAFoo(foo)
// CHECK-NEXT: ^~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'takeAFoo' unavailable (cannot import) 
// CHECK-NEXT: void takeAFoo(Foo *foo);
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: parameter 'foo' unavailable (cannot import)
// CHECK-NEXT: void takeAFoo(Foo *foo);
// CHECK-NEXT:               ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Foo' is incomplete
// CHECK-NEXT: void takeAFoo(Foo *foo);
// CHECK-NEXT:               ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Foo' is incomplete and cannot be imported as a stub; its name conflicts with a class in module CompleteSwiftTypes
// CHECK-NEXT: @class Foo;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Foo' forward declared here
// CHECK-NEXT: @class Foo;
// CHECK-NEXT: ^

_ = returnAFoo()
// CHECK:      incomplete_objc_types_swift_definition_imported.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'returnAFoo' in scope
// CHECK-NEXT: _ = returnAFoo()
// CHECK-NEXT:     ^~~~~~~~~~
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

takeABaz(bar)
// CHECK:      incomplete_objc_types_swift_definition_imported.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'takeABaz' in scope
// CHECK-NEXT: takeABaz(bar)
// CHECK-NEXT: ^~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'takeABaz' unavailable (cannot import) 
// CHECK-NEXT: void takeABaz(Baz *baz);
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: parameter 'baz' unavailable (cannot import)
// CHECK-NEXT: void takeABaz(Baz *baz);
// CHECK-NEXT:               ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Baz' is incomplete
// CHECK-NEXT: void takeABaz(Baz *baz);
// CHECK-NEXT:               ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Baz' is incomplete and cannot be imported as a stub; its name conflicts with a class in module CompleteSwiftTypes
// CHECK-NEXT: @class Baz;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Baz' forward declared here
// CHECK-NEXT: @class Baz;
// CHECK-NEXT: ^

_ = returnABaz()
// CHECK:     incomplete_objc_types_swift_definition_imported.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'returnABaz' in scope
// CHECK-NEXT: _ = returnABaz()
// CHECK-NEXT:     ^~~~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'returnABaz' unavailable (cannot import) 
// CHECK-NEXT: Baz *returnABaz();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: return type unavailable (cannot import)
// CHECK-NEXT: Baz *returnABaz();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Baz' is incomplete
// CHECK-NEXT: Baz *returnABaz();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Baz' is incomplete and cannot be imported as a stub; its name conflicts with a class in module CompleteSwiftTypes
// CHECK-NEXT: @class Baz;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Baz' forward declared here
// CHECK-NEXT: @class Baz;
// CHECK-NEXT: ^

takeASubscript(corge)
// CHECK:      incomplete_objc_types_swift_definition_imported.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'takeASubscript' in scope
// CHECK-NEXT: takeASubscript(corge)
// CHECK-NEXT: ^~~~~~~~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'takeASubscript' unavailable (cannot import) 
// CHECK-NEXT: void takeASubscript(subscript *param);
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: parameter 'param' unavailable (cannot import)
// CHECK-NEXT: void takeASubscript(subscript *param);
// CHECK-NEXT:                     ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'subscript' is incomplete
// CHECK-NEXT: void takeASubscript(subscript *param);
// CHECK-NEXT:                     ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'subscript' is incomplete and cannot be imported as a stub; its name conflicts with a class in module CompleteSwiftTypes
// CHECK-NEXT: @class subscript;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'subscript' forward declared here
// CHECK-NEXT: @class subscript;
// CHECK-NEXT: ^

_ = returnASubscript()
// CHECK:     incomplete_objc_types_swift_definition_imported.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'returnASubscript' in scope
// CHECK-NEXT: _ = returnASubscript()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'returnASubscript' unavailable (cannot import) 
// CHECK-NEXT: subscript* returnASubscript();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: return type unavailable (cannot import)
// CHECK-NEXT: subscript* returnASubscript();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'subscript' is incomplete
// CHECK-NEXT: subscript* returnASubscript();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'subscript' is incomplete and cannot be imported as a stub; its name conflicts with a class in module CompleteSwiftTypes
// CHECK-NEXT: @class subscript;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'subscript' forward declared here
// CHECK-NEXT: @class subscript;
// CHECK-NEXT: ^

_ = returnAProtocolFoo()
// CHECK:     incomplete_objc_types_swift_definition_imported.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'returnAProtocolFoo' in scope
// CHECK-NEXT: _ = returnAProtocolFoo()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'returnAProtocolFoo' unavailable (cannot import) 
// CHECK-NEXT: id<ProtocolFoo> returnAProtocolFoo();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: return type unavailable (cannot import)
// CHECK-NEXT: id<ProtocolFoo> returnAProtocolFoo();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'ProtocolFoo' is incomplete
// CHECK-NEXT: id<ProtocolFoo> returnAProtocolFoo();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'ProtocolFoo' is incomplete and cannot be imported as a stub; its name conflicts with a protocol in module CompleteSwiftTypes
// CHECK-NEXT: @protocol ProtocolFoo;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'ProtocolFoo' forward declared here
// CHECK-NEXT: @protocol ProtocolFoo;
// CHECK-NEXT: ^

_ = returnAProtocolBaz()
// CHECK:     incomplete_objc_types_swift_definition_imported.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'returnAProtocolBaz' in scope
// CHECK-NEXT: _ = returnAProtocolBaz()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'returnAProtocolBaz' unavailable (cannot import) 
// CHECK-NEXT: id<ProtocolBaz> returnAProtocolBaz();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: return type unavailable (cannot import)
// CHECK-NEXT: id<ProtocolBaz> returnAProtocolBaz();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'ProtocolBaz' is incomplete
// CHECK-NEXT: id<ProtocolBaz> returnAProtocolBaz();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'ProtocolBaz' is incomplete and cannot be imported as a stub; its name conflicts with a protocol in module CompleteSwiftTypes
// CHECK-NEXT: @protocol ProtocolBaz;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'ProtocolBaz' forward declared here
// CHECK-NEXT: @protocol ProtocolBaz;
// CHECK-NEXT: ^
