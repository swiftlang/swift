// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library %S/Inputs/custom-modules/IncompleteTypes/complete-swift-types.swift -module-name CompleteSwiftTypes -emit-module -emit-module-path %t/CompleteSwiftTypes.swiftmodule
// RUN: not %target-swift-frontend -enable-import-objc-forward-declarations -enable-objc-interop -typecheck -I %S/Inputs/custom-modules/IncompleteTypes -I %t %s 2>&1 | tee ~/two-one.txt | %FileCheck %s
// RUN: not %target-swift-frontend -swift-version 6 -enable-objc-interop -typecheck -I %S/Inputs/custom-modules/IncompleteTypes -I %t %s 2>&1 | tee ~/two-two.txt | %FileCheck  %s

// REQUIRES: objc_interop

import CompleteSwiftTypes
import ObjCLibraryForwardDeclaringCompleteSwiftTypes

let foo = Foo()
let bar = Bar()
let corge = Corge()

takeAFoo(foo)
// CHECK:      incomplete_objc_types_swift_definition_imported.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'takeAFoo' in scope
// CHECK-NEXT: takeAFoo(foo)
// CHECK-NEXT: ^~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'takeAFoo' not imported
// CHECK-NEXT: void takeAFoo(Foo *foo);
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: parameter 'foo' not imported
// CHECK-NEXT: void takeAFoo(Foo *foo);
// CHECK-NEXT:               ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Foo' is incomplete
// CHECK-NEXT: void takeAFoo(Foo *foo);
// CHECK-NEXT:               ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: incomplete interface 'Foo' shadows an imported @objc Swift class from module 'CompleteSwiftTypes'; import the generated Swift header for module 'CompleteSwiftTypes' in this header to realize this declaration
// CHECK-NEXT: @class Foo;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Foo' forward declared here
// CHECK-NEXT: @class Foo;
// CHECK-NEXT: ^

_ = returnAFoo()
// CHECK:      incomplete_objc_types_swift_definition_imported.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'returnAFoo' in scope
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

takeABaz(bar)
// CHECK:      incomplete_objc_types_swift_definition_imported.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'takeABaz' in scope
// CHECK-NEXT: takeABaz(bar)
// CHECK-NEXT: ^~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'takeABaz' not imported
// CHECK-NEXT: void takeABaz(Baz *baz);
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: parameter 'baz' not imported
// CHECK-NEXT: void takeABaz(Baz *baz);
// CHECK-NEXT:               ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Baz' is incomplete
// CHECK-NEXT: void takeABaz(Baz *baz);
// CHECK-NEXT:               ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: incomplete interface 'Baz' shadows an imported @objc Swift class from module 'CompleteSwiftTypes'; import the generated Swift header for module 'CompleteSwiftTypes' in this header to realize this declaration
// CHECK-NEXT: @class Baz;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Baz' forward declared here
// CHECK-NEXT: @class Baz;
// CHECK-NEXT: ^

_ = returnABaz()
// CHECK:     incomplete_objc_types_swift_definition_imported.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'returnABaz' in scope
// CHECK-NEXT: _ = returnABaz()
// CHECK-NEXT:     ^~~~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'returnABaz' not imported
// CHECK-NEXT: Baz *returnABaz();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: return type not imported
// CHECK-NEXT: Baz *returnABaz();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Baz' is incomplete
// CHECK-NEXT: Baz *returnABaz();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: incomplete interface 'Baz' shadows an imported @objc Swift class from module 'CompleteSwiftTypes'; import the generated Swift header for module 'CompleteSwiftTypes' in this header to realize this declaration
// CHECK-NEXT: @class Baz;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'Baz' forward declared here
// CHECK-NEXT: @class Baz;
// CHECK-NEXT: ^

takeASubscript(corge)
// CHECK:      incomplete_objc_types_swift_definition_imported.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'takeASubscript' in scope
// CHECK-NEXT: takeASubscript(corge)
// CHECK-NEXT: ^~~~~~~~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'takeASubscript' not imported
// CHECK-NEXT: void takeASubscript(subscript *param);
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: parameter 'param' not imported
// CHECK-NEXT: void takeASubscript(subscript *param);
// CHECK-NEXT:                     ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'subscript' is incomplete
// CHECK-NEXT: void takeASubscript(subscript *param);
// CHECK-NEXT:                     ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: incomplete interface 'subscript' shadows an imported @objc Swift class from module 'CompleteSwiftTypes'; import the generated Swift header for module 'CompleteSwiftTypes' in this header to realize this declaration
// CHECK-NEXT: @class subscript;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'subscript' forward declared here
// CHECK-NEXT: @class subscript;
// CHECK-NEXT: ^

_ = returnASubscript()
// CHECK:     incomplete_objc_types_swift_definition_imported.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'returnASubscript' in scope
// CHECK-NEXT: _ = returnASubscript()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'returnASubscript' not imported
// CHECK-NEXT: subscript* returnASubscript();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: return type not imported
// CHECK-NEXT: subscript* returnASubscript();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'subscript' is incomplete
// CHECK-NEXT: subscript* returnASubscript();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: incomplete interface 'subscript' shadows an imported @objc Swift class from module 'CompleteSwiftTypes'; import the generated Swift header for module 'CompleteSwiftTypes' in this header to realize this declaration
// CHECK-NEXT: @class subscript;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'subscript' forward declared here
// CHECK-NEXT: @class subscript;
// CHECK-NEXT: ^

_ = returnAProtocolFoo()
// CHECK:     incomplete_objc_types_swift_definition_imported.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'returnAProtocolFoo' in scope
// CHECK-NEXT: _ = returnAProtocolFoo()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'returnAProtocolFoo' not imported
// CHECK-NEXT: id<ProtocolFoo> returnAProtocolFoo();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: return type not imported
// CHECK-NEXT: id<ProtocolFoo> returnAProtocolFoo();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'ProtocolFoo' is incomplete
// CHECK-NEXT: id<ProtocolFoo> returnAProtocolFoo();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: incomplete protocol 'ProtocolFoo' shadows an imported @objc Swift protocol from module 'CompleteSwiftTypes'; import the generated Swift header for module 'CompleteSwiftTypes' in this header to realize this declaration
// CHECK-NEXT: @protocol ProtocolFoo;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'ProtocolFoo' forward declared here
// CHECK-NEXT: @protocol ProtocolFoo;
// CHECK-NEXT: ^

_ = returnAProtocolBaz()
// CHECK:     incomplete_objc_types_swift_definition_imported.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'returnAProtocolBaz' in scope
// CHECK-NEXT: _ = returnAProtocolBaz()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'returnAProtocolBaz' not imported
// CHECK-NEXT: id<ProtocolBaz> returnAProtocolBaz();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: return type not imported
// CHECK-NEXT: id<ProtocolBaz> returnAProtocolBaz();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'ProtocolBaz' is incomplete
// CHECK-NEXT: id<ProtocolBaz> returnAProtocolBaz();
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: incomplete protocol 'ProtocolBaz' shadows an imported @objc Swift protocol from module 'CompleteSwiftTypes'; import the generated Swift header for module 'CompleteSwiftTypes' in this header to realize this declaration
// CHECK-NEXT: @protocol ProtocolBaz;
// CHECK-NEXT: ^
// CHECK-NEXT: objc-library-forward-declaring-complete-swift-types.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'ProtocolBaz' forward declared here
// CHECK-NEXT: @protocol ProtocolBaz;
// CHECK-NEXT: ^
