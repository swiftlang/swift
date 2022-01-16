// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-experimental-clang-importer-diagnostics -enable-objc-interop -typecheck %s 2>&1 | %FileCheck %s --strict-whitespace

// REQUIRES: objc_interop

import IncompleteTypes

let foo = Foo()
_ = foo.methodReturningCompleteInterface()
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: warning: method 'methodReturningCompleteInterface' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: return type not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' forward declared here
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: value of type 'Foo' has no member 'methodReturningCompleteInterface'

_ = foo.methodTakingACompleteInterfaceAsAParameter(nil, andAnother: nil)
// CHECK-NOT:     IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: warning: method 'methodTakingACompleteInterfaceAsAParameter:andAnother:' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: parameter 'param1' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' forward declared here
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: value of type 'Foo' has no member 'methodTakingACompleteInterfaceAsAParameter'

foo.propertyUsingACompleteInterface = nil
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: warning: property 'propertyUsingACompleteInterface' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' forward declared here
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: value of type 'Foo' has no member 'propertyUsingACompleteInterface'

_ = CFunctionReturningACompleteInterface()
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: warning: function 'CFunctionReturningACompleteInterface' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: return type not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' forward declared here
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'CFunctionReturningACompleteInterface' in scope

CFunctionTakingACompleteInterfaceAsAParameter(nil)
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: warning: function 'CFunctionTakingACompleteInterfaceAsAParameter' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: parameter 'param1' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'CompleteInterface' forward declared here
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'CFunctionTakingACompleteInterfaceAsAParameter' in scope

_ = foo.methodReturningCompleteProtocol()
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: warning: method 'methodReturningCompleteProtocol' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: return type not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' forward declared here
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: value of type 'Foo' has no member 'methodReturningCompleteProtocol'

_ = foo.methodTakingACompleteProtocolAsAParameter(nil)
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: warning: method 'methodTakingACompleteProtocolAsAParameter:' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: parameter 'param1' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' forward declared here
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: value of type 'Foo' has no member 'methodTakingACompleteProtocolAsAParameter'

foo.propertyUsingACompleteProtocol = nil
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: warning: property 'propertyUsingACompleteProtocol' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' forward declared here
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: value of type 'Foo' has no member 'propertyUsingACompleteProtocol'

_ = CFunctionReturningACompleteProtocol()
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: warning: function 'CFunctionReturningACompleteProtocol' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: return type not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' forward declared here
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'CFunctionReturningACompleteProtocol' in scope

CFunctionTakingACompleteProtocolAsAParameter(nil)
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: warning: function 'CFunctionTakingACompleteProtocolAsAParameter' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: parameter 'param1' not imported
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' is incomplete
// CHECK-NOT: IncompleteTypes.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'CompleteProtocol' forward declared here
// CHECK-NOT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:{{[0-9]+}}: error: cannot find 'CFunctionTakingACompleteProtocolAsAParameter' in scope
