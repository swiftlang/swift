// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -swift-version 5 -enable-objc-interop -typecheck %s -diagnostic-style llvm 2>&1 | %FileCheck %s --strict-whitespace

// REQUIRES: objc_interop

import IncompleteTypes

let bar = Bar()
_ = bar.methodReturningForwardDeclaredInterface()
// CHECK:      experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:9: error: value of type 'Bar' has no member 'methodReturningForwardDeclaredInterface'
// CHECK-NEXT: _ = bar.methodReturningForwardDeclaredInterface()
// CHECK-NEXT:     ~~~ ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: method 'methodReturningForwardDeclaredInterface' unavailable (cannot import)
// CHECK-NEXT: - (ForwardDeclaredInterface *) methodReturningForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: return type unavailable (cannot import)
// CHECK-NEXT: - (ForwardDeclaredInterface *) methodReturningForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' is incomplete
// CHECK-NEXT: - (ForwardDeclaredInterface *) methodReturningForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' forward declared here
// CHECK-NEXT: @class ForwardDeclaredInterface;
// CHECK-NEXT: ^

_ = bar.methodTakingAForwardDeclaredInterfaceAsAParameter(nil, andAnother: nil)
// CHECK:      experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:9: error: value of type 'Bar' has no member 'methodTakingAForwardDeclaredInterfaceAsAParameter'
// CHECK-NEXT: _ = bar.methodTakingAForwardDeclaredInterfaceAsAParameter(nil, andAnother: nil)
// CHECK-NEXT:     ~~~ ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: method 'methodTakingAForwardDeclaredInterfaceAsAParameter:andAnother:' unavailable (cannot import)
// CHECK-NEXT: - (int)methodTakingAForwardDeclaredInterfaceAsAParameter:(ForwardDeclaredInterface *)param1 andAnother:(ForwardDeclaredInterface *)param2;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:59: note: parameter 'param1' unavailable (cannot import)
// CHECK-NEXT: - (int)methodTakingAForwardDeclaredInterfaceAsAParameter:(ForwardDeclaredInterface *)param1 andAnother:(ForwardDeclaredInterface *)param2;
// CHECK-NEXT:                                                           ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:59: note: interface 'ForwardDeclaredInterface' is incomplete
// CHECK-NEXT: - (int)methodTakingAForwardDeclaredInterfaceAsAParameter:(ForwardDeclaredInterface *)param1 andAnother:(ForwardDeclaredInterface *)param2;
// CHECK-NEXT:                                                           ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' forward declared here
// CHECK-NEXT: @class ForwardDeclaredInterface;
// CHECK-NEXT: ^

bar.propertyUsingAForwardDeclaredInterface = nil
// CHECK:      experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:5: error: value of type 'Bar' has no member 'propertyUsingAForwardDeclaredInterface'
// CHECK-NEXT: bar.propertyUsingAForwardDeclaredInterface = nil
// CHECK-NEXT: ~~~ ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: property 'propertyUsingAForwardDeclaredInterface' unavailable (cannot import)
// CHECK-NEXT: @property ForwardDeclaredInterface* propertyUsingAForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' is incomplete
// CHECK-NEXT: @property ForwardDeclaredInterface* propertyUsingAForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' forward declared here
// CHECK-NEXT: @class ForwardDeclaredInterface;
// CHECK-NEXT: ^

_ = CFunctionReturningAForwardDeclaredInterface()
// CHECK:      experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:5: error: cannot find 'CFunctionReturningAForwardDeclaredInterface' in scope
// CHECK-NEXT: _ = CFunctionReturningAForwardDeclaredInterface()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: function 'CFunctionReturningAForwardDeclaredInterface' unavailable (cannot import)
// CHECK-NEXT: ForwardDeclaredInterface* CFunctionReturningAForwardDeclaredInterface();
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: return type unavailable (cannot import)
// CHECK-NEXT: ForwardDeclaredInterface* CFunctionReturningAForwardDeclaredInterface();
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' is incomplete
// CHECK-NEXT: ForwardDeclaredInterface* CFunctionReturningAForwardDeclaredInterface();
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' forward declared here
// CHECK-NEXT: @class ForwardDeclaredInterface;
// CHECK-NEXT: ^

CFunctionTakingAForwardDeclaredInterfaceAsAParameter(nil)
// CHECK:      experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:1: error: cannot find 'CFunctionTakingAForwardDeclaredInterfaceAsAParameter' in scope
// CHECK-NEXT: CFunctionTakingAForwardDeclaredInterfaceAsAParameter(nil)
// CHECK-NEXT: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: function 'CFunctionTakingAForwardDeclaredInterfaceAsAParameter' unavailable (cannot import)
// CHECK-NEXT: void CFunctionTakingAForwardDeclaredInterfaceAsAParameter(ForwardDeclaredInterface* param1);
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:59: note: parameter 'param1' unavailable (cannot import)
// CHECK-NEXT: void CFunctionTakingAForwardDeclaredInterfaceAsAParameter(ForwardDeclaredInterface* param1);
// CHECK-NEXT:                                                           ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:59: note: interface 'ForwardDeclaredInterface' is incomplete
// CHECK-NEXT: void CFunctionTakingAForwardDeclaredInterfaceAsAParameter(ForwardDeclaredInterface* param1);
// CHECK-NEXT:                                                           ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' forward declared here
// CHECK-NEXT: @class ForwardDeclaredInterface;
// CHECK-NEXT: ^

_ = bar.methodReturningForwardDeclaredProtocol()
// CHECK:      experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:9: error: value of type 'Bar' has no member 'methodReturningForwardDeclaredProtocol'
// CHECK-NEXT: _ = bar.methodReturningForwardDeclaredProtocol()
// CHECK-NEXT:     ~~~ ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: method 'methodReturningForwardDeclaredProtocol' unavailable (cannot import)
// CHECK-NEXT: - (NSObject<ForwardDeclaredProtocol> *) methodReturningForwardDeclaredProtocol;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: return type unavailable (cannot import)
// CHECK-NEXT: - (NSObject<ForwardDeclaredProtocol> *) methodReturningForwardDeclaredProtocol;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' is incomplete
// CHECK-NEXT: - (NSObject<ForwardDeclaredProtocol> *) methodReturningForwardDeclaredProtocol;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' forward declared here
// CHECK-NEXT: @protocol ForwardDeclaredProtocol;
// CHECK-NEXT: ^

_ = bar.methodTakingAForwardDeclaredProtocolAsAParameter(nil)
// CHECK:      experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:9: error: value of type 'Bar' has no member 'methodTakingAForwardDeclaredProtocolAsAParameter'
// CHECK-NEXT: _ = bar.methodTakingAForwardDeclaredProtocolAsAParameter(nil)
// CHECK-NEXT:     ~~~ ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: method 'methodTakingAForwardDeclaredProtocolAsAParameter:' unavailable (cannot import)
// CHECK-NEXT: - (int)methodTakingAForwardDeclaredProtocolAsAParameter:(id<ForwardDeclaredProtocol>)param1;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:58: note: parameter 'param1' unavailable (cannot import)
// CHECK-NEXT: - (int)methodTakingAForwardDeclaredProtocolAsAParameter:(id<ForwardDeclaredProtocol>)param1;
// CHECK-NEXT:                                                          ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:58: note: protocol 'ForwardDeclaredProtocol' is incomplete
// CHECK-NEXT: - (int)methodTakingAForwardDeclaredProtocolAsAParameter:(id<ForwardDeclaredProtocol>)param1;
// CHECK-NEXT:                                                          ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' forward declared here
// CHECK-NEXT: @protocol ForwardDeclaredProtocol;
// CHECK-NEXT: ^

bar.propertyUsingAForwardDeclaredProtocol = nil
// CHECK:      experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:5: error: value of type 'Bar' has no member 'propertyUsingAForwardDeclaredProtocol'
// CHECK-NEXT: bar.propertyUsingAForwardDeclaredProtocol = nil
// CHECK-NEXT: ~~~ ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: property 'propertyUsingAForwardDeclaredProtocol' unavailable (cannot import)
// CHECK-NEXT: @property id<ForwardDeclaredProtocol> propertyUsingAForwardDeclaredProtocol;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' is incomplete
// CHECK-NEXT: @property id<ForwardDeclaredProtocol> propertyUsingAForwardDeclaredProtocol;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' forward declared here
// CHECK-NEXT: @protocol ForwardDeclaredProtocol;
// CHECK-NEXT: ^

_ = CFunctionReturningAForwardDeclaredProtocol()
// CHECK:      experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:5: error: cannot find 'CFunctionReturningAForwardDeclaredProtocol' in scope
// CHECK-NEXT: _ = CFunctionReturningAForwardDeclaredProtocol()
// CHECK-NEXT:     ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: function 'CFunctionReturningAForwardDeclaredProtocol' unavailable (cannot import)
// CHECK-NEXT: NSObject<ForwardDeclaredProtocol> *CFunctionReturningAForwardDeclaredProtocol();
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: return type unavailable (cannot import)
// CHECK-NEXT: NSObject<ForwardDeclaredProtocol> *CFunctionReturningAForwardDeclaredProtocol();
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' is incomplete
// CHECK-NEXT: NSObject<ForwardDeclaredProtocol> *CFunctionReturningAForwardDeclaredProtocol();
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' forward declared here
// CHECK-NEXT: @protocol ForwardDeclaredProtocol;
// CHECK-NEXT: ^

CFunctionTakingAForwardDeclaredProtocolAsAParameter(nil)
// CHECK: experimental_diagnostics_incomplete_types.swift:{{[0-9]+}}:1: error: cannot find 'CFunctionTakingAForwardDeclaredProtocolAsAParameter' in scope
// CHECK-NEXT: CFunctionTakingAForwardDeclaredProtocolAsAParameter(nil)
// CHECK-NEXT: ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: function 'CFunctionTakingAForwardDeclaredProtocolAsAParameter' unavailable (cannot import)
// CHECK-NEXT: void CFunctionTakingAForwardDeclaredProtocolAsAParameter(id<ForwardDeclaredProtocol> param1);
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:58: note: parameter 'param1' unavailable (cannot import)
// CHECK-NEXT: void CFunctionTakingAForwardDeclaredProtocolAsAParameter(id<ForwardDeclaredProtocol> param1);
// CHECK-NEXT:                                                          ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:58: note: protocol 'ForwardDeclaredProtocol' is incomplete
// CHECK-NEXT: void CFunctionTakingAForwardDeclaredProtocolAsAParameter(id<ForwardDeclaredProtocol> param1);
// CHECK-NEXT:                                                          ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' forward declared here
// CHECK-NEXT: @protocol ForwardDeclaredProtocol;
// CHECK-NEXT: ^
