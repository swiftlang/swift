// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -swift-version 5 -enable-objc-interop -typecheck %s 2>&1 | %FileCheck %s --strict-whitespace

// REQUIRES: objc_interop

import IncompleteTypes

let foo = Foo()
_ = foo.methodReturningCompleteInterface()
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: value of type 'Foo' has no member 'methodReturningCompleteInterface'
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: method 'methodReturningCompleteInterface' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: return type not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' forward declared here

_ = foo.methodTakingACompleteInterfaceAsAParameter(nil, andAnother: nil)
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: value of type 'Foo' has no member 'methodTakingACompleteInterfaceAsAParameter'
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: method 'methodTakingACompleteInterfaceAsAParameter:andAnother:' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: parameter 'param1' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' forward declared here

foo.propertyUsingACompleteInterface = nil
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: note: value of type 'Foo' has no member 'propertyUsingACompleteInterface'
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: warning: property 'propertyUsingACompleteInterface' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' forward declared here

_ = CFunctionReturningACompleteInterface()
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'CFunctionReturningACompleteInterface' in scope
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'CFunctionReturningACompleteInterface' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: return type not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' forward declared here

CFunctionTakingACompleteInterfaceAsAParameter(nil)
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'CFunctionTakingACompleteInterfaceAsAParameter' in scope
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'CFunctionTakingACompleteInterfaceAsAParameter' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: parameter 'param1' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' forward declared here

_ = foo.methodReturningCompleteProtocol()
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: value of type 'Foo' has no member 'methodReturningCompleteProtocol'
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: method 'methodReturningCompleteProtocol' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: return type not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' forward declared here

_ = foo.methodTakingACompleteProtocolAsAParameter(nil)
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: value of type 'Foo' has no member 'methodTakingACompleteProtocolAsAParameter'
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: method 'methodTakingACompleteProtocolAsAParameter:' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: parameter 'param1' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' forward declared here

foo.propertyUsingACompleteProtocol = nil
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: note: value of type 'Foo' has no member 'propertyUsingACompleteProtocol'
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: warning: property 'propertyUsingACompleteProtocol' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' forward declared here

_ = CFunctionReturningACompleteProtocol()
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'CFunctionReturningACompleteProtocol' in scope
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'CFunctionReturningACompleteProtocol' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: return type not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' forward declared here

CFunctionTakingACompleteProtocolAsAParameter(nil)
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'CFunctionTakingACompleteProtocolAsAParameter' in scope
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: function 'CFunctionTakingACompleteProtocolAsAParameter' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: parameter 'param1' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' forward declared here
