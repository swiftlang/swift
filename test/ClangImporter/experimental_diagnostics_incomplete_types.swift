// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-experimental-clang-importer-diagnostics -enable-objc-interop -typecheck %s 2>&1 | %FileCheck %s --strict-whitespace

// REQUIRES: objc_interop

import IncompleteTypes

let bar = Bar()
_ = bar.methodReturningForwardDeclaredInterface()
// CHECK: IncompleteTypes.h:{{[0-9]+}}:1: warning: method 'methodReturningForwardDeclaredInterface' not imported
// CHECK-NEXT: - (ForwardDeclaredInterface *) methodReturningForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: return type not imported
// CHECK-NEXT: - (ForwardDeclaredInterface *) methodReturningForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' is incomplete
// CHECK-NEXT: - (ForwardDeclaredInterface *) methodReturningForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' forward declared here
// CHECK-NEXT: @class ForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:9: error: value of type 'Bar' has no member 'methodReturningForwardDeclaredInterface'
// CHECK-NEXT: _ = bar.methodReturningForwardDeclaredInterface()
// CHECK-NEXT:     ~~~ ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

_ = bar.methodTakingAForwardDeclaredInterfaceAsAParameter(nil, andAnother: nil)
// CHECK:     IncompleteTypes.h:{{[0-9]+}}:1: warning: method 'methodTakingAForwardDeclaredInterfaceAsAParameter:andAnother:' not imported
// CHECK-NEXT: - (int)methodTakingAForwardDeclaredInterfaceAsAParameter:(ForwardDeclaredInterface *)param1 andAnother:(ForwardDeclaredInterface *)param2;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:59: note: parameter 'param1' not imported
// CHECK-NEXT: - (int)methodTakingAForwardDeclaredInterfaceAsAParameter:(ForwardDeclaredInterface *)param1 andAnother:(ForwardDeclaredInterface *)param2;
// CHECK-NEXT:                                                           ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:59: note: interface 'ForwardDeclaredInterface' is incomplete
// CHECK-NEXT: - (int)methodTakingAForwardDeclaredInterfaceAsAParameter:(ForwardDeclaredInterface *)param1 andAnother:(ForwardDeclaredInterface *)param2;
// CHECK-NEXT:                                                           ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' forward declared here
// CHECK-NEXT: @class ForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:9: error: value of type 'Bar' has no member 'methodTakingAForwardDeclaredInterfaceAsAParameter'
// CHECK-NEXT: _ = bar.methodTakingAForwardDeclaredInterfaceAsAParameter(nil, andAnother: nil)
// CHECK-NEXT:     ~~~ ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bar.propertyUsingAForwardDeclaredInterface = nil
// CHECK: IncompleteTypes.h:{{[0-9]+}}:1: warning: property 'propertyUsingAForwardDeclaredInterface' not imported
// CHECK-NEXT: @property ForwardDeclaredInterface* propertyUsingAForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' is incomplete
// CHECK-NEXT: @property ForwardDeclaredInterface* propertyUsingAForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' forward declared here
// CHECK-NEXT: @class ForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:5: error: value of type 'Bar' has no member 'propertyUsingAForwardDeclaredInterface'
// CHECK-NEXT: bar.propertyUsingAForwardDeclaredInterface = nil
// CHECK-NEXT: ~~~ ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

_ = CFunctionReturningAForwardDeclaredInterface()
// CHECK: IncompleteTypes.h:{{[0-9]+}}:1: warning: function 'CFunctionReturningAForwardDeclaredInterface' not imported
// CHECK-NEXT: ForwardDeclaredInterface* CFunctionReturningAForwardDeclaredInterface();
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: return type not imported
// CHECK-NEXT: ForwardDeclaredInterface* CFunctionReturningAForwardDeclaredInterface();
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' is incomplete
// CHECK-NEXT: ForwardDeclaredInterface* CFunctionReturningAForwardDeclaredInterface();
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' forward declared here
// CHECK-NEXT: @class ForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:5: error: cannot find 'CFunctionReturningAForwardDeclaredInterface' in scope
// CHECK-NEXT: _ = CFunctionReturningAForwardDeclaredInterface()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CFunctionTakingAForwardDeclaredInterfaceAsAParameter(nil)
// CHECK: IncompleteTypes.h:{{[0-9]+}}:1: warning: function 'CFunctionTakingAForwardDeclaredInterfaceAsAParameter' not imported
// CHECK-NEXT: void CFunctionTakingAForwardDeclaredInterfaceAsAParameter(ForwardDeclaredInterface* param1);
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:59: note: parameter 'param1' not imported
// CHECK-NEXT: void CFunctionTakingAForwardDeclaredInterfaceAsAParameter(ForwardDeclaredInterface* param1);
// CHECK-NEXT:                                                           ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:59: note: interface 'ForwardDeclaredInterface' is incomplete
// CHECK-NEXT: void CFunctionTakingAForwardDeclaredInterfaceAsAParameter(ForwardDeclaredInterface* param1);
// CHECK-NEXT:                                                           ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' forward declared here
// CHECK-NEXT: @class ForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:1: error: cannot find 'CFunctionTakingAForwardDeclaredInterfaceAsAParameter' in scope
// CHECK-NEXT: CFunctionTakingAForwardDeclaredInterfaceAsAParameter(nil)
// CHECK-NEXT: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

_ = bar.methodReturningForwardDeclaredProtocol()
// CHECK: IncompleteTypes.h:{{[0-9]+}}:1: warning: method 'methodReturningForwardDeclaredProtocol' not imported
// CHECK-NEXT: - (NSObject<ForwardDeclaredProtocol> *) methodReturningForwardDeclaredProtocol;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: return type not imported
// CHECK-NEXT: - (NSObject<ForwardDeclaredProtocol> *) methodReturningForwardDeclaredProtocol;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' is incomplete
// CHECK-NEXT: - (NSObject<ForwardDeclaredProtocol> *) methodReturningForwardDeclaredProtocol;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' forward declared here
// CHECK-NEXT: @protocol ForwardDeclaredProtocol;
// CHECK-NEXT: ^
// CHECK-NEXT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:9: error: value of type 'Bar' has no member 'methodReturningForwardDeclaredProtocol'
// CHECK-NEXT: _ = bar.methodReturningForwardDeclaredProtocol()
// CHECK-NEXT:     ~~~ ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

_ = bar.methodTakingAForwardDeclaredProtocolAsAParameter(nil)
// CHECK:     IncompleteTypes.h:{{[0-9]+}}:1: warning: method 'methodTakingAForwardDeclaredProtocolAsAParameter:' not imported
// CHECK-NEXT: - (int)methodTakingAForwardDeclaredProtocolAsAParameter:(id<ForwardDeclaredProtocol>)param1;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:58: note: parameter 'param1' not imported
// CHECK-NEXT: - (int)methodTakingAForwardDeclaredProtocolAsAParameter:(id<ForwardDeclaredProtocol>)param1;
// CHECK-NEXT:                                                          ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:58: note: protocol 'ForwardDeclaredProtocol' is incomplete
// CHECK-NEXT: - (int)methodTakingAForwardDeclaredProtocolAsAParameter:(id<ForwardDeclaredProtocol>)param1;
// CHECK-NEXT:                                                          ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' forward declared here
// CHECK-NEXT: @protocol ForwardDeclaredProtocol;
// CHECK-NEXT: ^
// CHECK-NEXT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:9: error: value of type 'Bar' has no member 'methodTakingAForwardDeclaredProtocolAsAParameter'
// CHECK-NEXT: _ = bar.methodTakingAForwardDeclaredProtocolAsAParameter(nil)
// CHECK-NEXT:     ~~~ ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bar.propertyUsingAForwardDeclaredProtocol = nil
// CHECK: IncompleteTypes.h:{{[0-9]+}}:1: warning: property 'propertyUsingAForwardDeclaredProtocol' not imported
// CHECK-NEXT: @property id<ForwardDeclaredProtocol> propertyUsingAForwardDeclaredProtocol;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' is incomplete
// CHECK-NEXT: @property id<ForwardDeclaredProtocol> propertyUsingAForwardDeclaredProtocol;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' forward declared here
// CHECK-NEXT: @protocol ForwardDeclaredProtocol;
// CHECK-NEXT: ^
// CHECK-NEXT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:5: error: value of type 'Bar' has no member 'propertyUsingAForwardDeclaredProtocol'
// CHECK-NEXT: bar.propertyUsingAForwardDeclaredProtocol = nil
// CHECK-NEXT: ~~~ ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

_ = CFunctionReturningAForwardDeclaredProtocol()
// CHECK: IncompleteTypes.h:{{[0-9]+}}:1: warning: function 'CFunctionReturningAForwardDeclaredProtocol' not imported
// CHECK-NEXT: NSObject<ForwardDeclaredProtocol> *CFunctionReturningAForwardDeclaredProtocol();
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: return type not imported
// CHECK-NEXT: NSObject<ForwardDeclaredProtocol> *CFunctionReturningAForwardDeclaredProtocol();
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' is incomplete
// CHECK-NEXT: NSObject<ForwardDeclaredProtocol> *CFunctionReturningAForwardDeclaredProtocol();
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' forward declared here
// CHECK-NEXT: @protocol ForwardDeclaredProtocol;
// CHECK-NEXT: ^
// CHECK-NEXT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:5: error: cannot find 'CFunctionReturningAForwardDeclaredProtocol' in scope
// CHECK-NEXT: _ = CFunctionReturningAForwardDeclaredProtocol()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CFunctionTakingAForwardDeclaredProtocolAsAParameter(nil)
// CHECK: IncompleteTypes.h:{{[0-9]+}}:1: warning: function 'CFunctionTakingAForwardDeclaredProtocolAsAParameter' not imported
// CHECK-NEXT: void CFunctionTakingAForwardDeclaredProtocolAsAParameter(id<ForwardDeclaredProtocol> param1);
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:58: note: parameter 'param1' not imported
// CHECK-NEXT: void CFunctionTakingAForwardDeclaredProtocolAsAParameter(id<ForwardDeclaredProtocol> param1);
// CHECK-NEXT:                                                          ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:58: note: protocol 'ForwardDeclaredProtocol' is incomplete
// CHECK-NEXT: void CFunctionTakingAForwardDeclaredProtocolAsAParameter(id<ForwardDeclaredProtocol> param1);
// CHECK-NEXT:                                                          ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' forward declared here
// CHECK-NEXT: @protocol ForwardDeclaredProtocol;
// CHECK-NEXT: ^
// CHECK-NEXT: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:1: error: cannot find 'CFunctionTakingAForwardDeclaredProtocolAsAParameter' in scope
// CHECK-NEXT: CFunctionTakingAForwardDeclaredProtocolAsAParameter(nil)
// CHECK-NEXT: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
