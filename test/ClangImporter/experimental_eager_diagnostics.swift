// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-experimental-eager-clang-module-diagnostics -enable-objc-interop -typecheck %s 2>&1 | %FileCheck %s --strict-whitespace

// REQUIRES: objc_interop

import cfuncs
import ctypes
import IncompleteTypes
import macros

// C Functions

// CHECK:      cfuncs.h:{{[0-9]+}}:1: note: function 'unsupported_parameter_type' not imported
// CHECK-NEXT:      int unsupported_parameter_type(int param1, _Complex int param2);
// CHECK-NEXT: {{^}}^
// CHECK-NEXT: cfuncs.h:{{[0-9]+}}:44: note: parameter 'param2' not imported
// CHECK-NEXT:      int unsupported_parameter_type(int param1, _Complex int param2);
// CHECK-NEXT: {{^}}                                           ^
// CHECK-NEXT: cfuncs.h:{{[0-9]+}}:44: note: built-in type 'Complex' not supported
// CHECK-NEXT:      int unsupported_parameter_type(int param1, _Complex int param2);
// CHECK-NEXT: {{^}}                                           ^

// CHECK: cfuncs.h:{{[0-9]+}}:1: note: function 'unsupported_return_type' not imported
// CHECK-NEXT:      _Complex int unsupported_return_type();
// CHECK-NEXT: {{^}}^
// CHECK-NEXT: cfuncs.h:{{[0-9]+}}:1: note: return type not imported
// CHECK-NEXT:      _Complex int unsupported_return_type();
// CHECK-NEXT: {{^}}^
// CHECK-NEXT: cfuncs.h:{{[0-9]+}}:1: note: built-in type 'Complex' not supported
// CHECK-NEXT:      _Complex int unsupported_return_type();
// CHECK-NEXT: {{^}}^

// C structs

// CHECK:      ctypes.h:{{[0-9]+}}:3: note: field 'c' not imported
// CHECK-NEXT:   int _Complex c;
// CHECK-NEXT:   ^
// CHECK-NEXT: ctypes.h:{{[0-9]+}}:3: note: built-in type 'Complex' not supported
// CHECK-NEXT:   int _Complex c;
// CHECK-NEXT:   ^

// CHECK:      ctypes.h:{{[0-9]+}}:3: note: field 'd' not imported
// CHECK-NEXT:   int _Complex d;
// CHECK-NEXT:   ^
// CHECK-NEXT: ctypes.h:{{[0-9]+}}:3: note: built-in type 'Complex' not supported
// CHECK-NEXT:   int _Complex d;
// CHECK-NEXT:   ^

// Incomplete types

// CHECK: IncompleteTypes.h:{{[0-9]+}}:1: note: property 'propertyUsingAForwardDeclaredProtocol' not imported
// CHECK-NEXT: @property id<ForwardDeclaredProtocol> propertyUsingAForwardDeclaredProtocol;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' is incomplete
// CHECK-NEXT: @property id<ForwardDeclaredProtocol> propertyUsingAForwardDeclaredProtocol;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: protocol 'ForwardDeclaredProtocol' forward declared here
// CHECK-NEXT: @protocol ForwardDeclaredProtocol;
// CHECK-NEXT: ^

// CHECK: IncompleteTypes.h:{{[0-9]+}}:1: note: property 'propertyUsingAForwardDeclaredInterface' not imported
// CHECK-NEXT: @property ForwardDeclaredInterface* propertyUsingAForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' is incomplete
// CHECK-NEXT: @property ForwardDeclaredInterface* propertyUsingAForwardDeclaredInterface;
// CHECK-NEXT: ^
// CHECK-NEXT: IncompleteTypes.h:{{[0-9]+}}:1: note: interface 'ForwardDeclaredInterface' forward declared here
// CHECK-NEXT: @class ForwardDeclaredInterface;
// CHECK-NEXT: ^

// CHECK: IncompleteTypes.h:{{[0-9]+}}:1: note: method 'methodReturningForwardDeclaredProtocol' not imported
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

// CHECK: IncompleteTypes.h:{{[0-9]+}}:1: note: method 'methodReturningForwardDeclaredInterface' not imported
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

// CHECK:     IncompleteTypes.h:{{[0-9]+}}:1: note: method 'methodTakingAForwardDeclaredProtocolAsAParameter:' not imported
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

// CHECK:     IncompleteTypes.h:{{[0-9]+}}:1: note: method 'methodTakingAForwardDeclaredInterfaceAsAParameter:andAnother:' not imported
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

// CHECK: IncompleteTypes.h:{{[0-9]+}}:1: note: function 'CFunctionReturningAForwardDeclaredInterface' not imported
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

// CHECK: IncompleteTypes.h:{{[0-9]+}}:1: note: function 'CFunctionReturningAForwardDeclaredProtocol' not imported
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

// CHECK: IncompleteTypes.h:{{[0-9]+}}:1: note: function 'CFunctionTakingAForwardDeclaredInterfaceAsAParameter' not imported
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

// CHECK: IncompleteTypes.h:{{[0-9]+}}:1: note: function 'CFunctionTakingAForwardDeclaredProtocolAsAParameter' not imported
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

// Macros

// CHECK:      macros.h:{{[0-9]+}}:9: note: macro 'CHAR_LITERAL' not imported
// CHECK-NEXT:      #define CHAR_LITERAL 'a'
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: macros.h:{{[0-9]+}}:22: note: only numeric and string macro literals supported
// CHECK-NEXT:      #define CHAR_LITERAL 'a'
// CHECK-NEXT: {{^}}                     ^

// CHECK:      macros.h:{{[0-9]+}}:9: note: macro 'FUNC_LIKE_MACRO' not imported: function like macros not supported
// CHECK-NEXT:      #define FUNC_LIKE_MACRO() 0
// CHECK-NEXT: {{^}}        ^

// CHECK:      macros.h:{{[0-9]+}}:9: note: macro 'INVALID_ARITHMETIC_1' not imported: structure not supported
// CHECK-NEXT:      #define INVALID_ARITHMETIC_1 5 + INVALID_INTEGER_LITERAL_1
// CHECK-NEXT: {{^}}        ^

// CHECK:      macros.h:{{[0-9]+}}:9: note: macro 'INVALID_ARITHMETIC_2' not imported: structure not supported
// CHECK-NEXT:      #define INVALID_ARITHMETIC_2 INVALID_INTEGER_LITERAL_1 + ADD_TWO
// CHECK-NEXT: {{^}}        ^

// CHECK:      macros.h:{{[0-9]+}}:9: note: macro 'INVALID_ARITHMETIC_3' not imported: structure not supported
// CHECK-NEXT:      #define INVALID_ARITHMETIC_3 ADD_TWO + INVALID_INTEGER_LITERAL_1
// CHECK-NEXT: {{^}}        ^

// CHECK:      macros.h:{{[0-9]+}}:9: note: macro 'INVALID_ARITHMETIC_4' not imported: structure not supported
// CHECK-NEXT:      #define INVALID_ARITHMETIC_4                                                   \
// CHECK-NEXT: {{^}}        ^

// CHECK:      macros.h:{{[0-9]+}}:9: note: macro 'INVALID_ARITHMETIC_5' not imported: structure not supported
// CHECK-NEXT:      #define INVALID_ARITHMETIC_5 1 + VERSION_STRING
// CHECK-NEXT: {{^}}        ^

// CHECK:      macros.h:{{[0-9]+}}:9: note: macro 'INVALID_ARITHMETIC_6' not imported: structure not supported
// CHECK-NEXT:      #define INVALID_ARITHMETIC_6 1 + 'c'
// CHECK-NEXT: {{^}}        ^

// CHECK:      macros.h:{{[0-9]+}}:9: note: macro 'INVALID_ARITHMETIC_7' not imported
// CHECK-NEXT:      #define INVALID_ARITHMETIC_7 3 % 2
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: macros.h:{{[0-9]+}}:32: note: operator '%' not supported in macro arithmetic
// CHECK-NEXT:      #define INVALID_ARITHMETIC_7 3 % 2
// CHECK-NEXT: {{^}}                               ^

// CHECK:      macros.h:{{[0-9]+}}:9: note: macro 'INVALID_INTEGER_LITERAL_2' not imported
// CHECK-NEXT:      #define INVALID_INTEGER_LITERAL_2 10abc
// CHECK-NEXT: {{^}}        ^
// CHECK-NEXT: macros.h:{{[0-9]+}}:35: note: invalid numeric literal
// CHECK-NEXT:      #define INVALID_INTEGER_LITERAL_2 10abc
// CHECK-NEXT: {{^}}                                  ^


// CHECK:      macros.h:{{[0-9]+}}:9: note: macro 'UNSUPPORTED_1' not imported: structure not supported
// CHECK-NEXT:      #define UNSUPPORTED_1 FUNC_LIKE_MACRO()
// CHECK-NEXT: {{^}}        ^

// CHECK:      macros.h:{{[0-9]+}}:9: note: macro 'UNSUPPORTED_2' not imported: structure not supported
// CHECK-NEXT:      #define UNSUPPORTED_2 FUNC_LIKE_MACRO_2(1)
// CHECK-NEXT: {{^}}        ^

// CHECK:      macros.h:{{[0-9]+}}:9: note: macro 'UNSUPPORTED_3' not imported: structure not supported
// CHECK-NEXT:      #define UNSUPPORTED_3 1 + FUNC_LIKE_MACRO_2(1)
// CHECK-NEXT: {{^}}        ^

// CHECK:      macros.h:{{[0-9]+}}:9: note: macro 'UNSUPPORTED_4' not imported: structure not supported
// CHECK-NEXT:      #define UNSUPPORTED_4                                                          \
// CHECK-NEXT: {{^}}        ^

// CHECK:      macros.h:{{[0-9]+}}:9: note: macro 'UNSUPPORTED_5' not imported: structure not supported
// CHECK-NEXT:      #define UNSUPPORTED_5 1 + 1 + 1
// CHECK-NEXT: {{^}}        ^
