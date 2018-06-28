// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/secret)
// RUN: %target-swift-frontend -emit-module -o %t/secret %S/Inputs/struct_with_operators.swift
// RUN: %empty-directory(%t/Frameworks/has_alias.framework/Modules/has_alias.swiftmodule)
// RUN: %target-swift-frontend -emit-module -o %t/Frameworks/has_alias.framework/Modules/has_alias.swiftmodule/%target-swiftmodule-name %S/Inputs/alias.swift -module-name has_alias

// RUN: cd %t && %target-swift-frontend -emit-module -o %t -I %t/secret -F %t/Frameworks -Fsystem %t/SystemFrameworks -debug-prefix-map %t=REMAPPED %S/Inputs/has_xref.swift
// RUN: %target-swift-frontend %s -typecheck -I %t -verify -show-diagnostics-after-fatal
// RUN: llvm-bcanalyzer -dump %t/has_xref.swiftmodule | %FileCheck %s

import has_xref // expected-error {{missing required modules: 'has_alias', 'struct_with_operators'}}

numeric(42) // expected-error {{use of unresolved identifier 'numeric'}}

// CHECK-LABEL: <OPTIONS_BLOCK
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = '-working-directory'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = 'REMAPPED'
// CHECK-NOT: <XCC abbrevid={{[0-9]+}}/> blob data = '-fdebug-prefix-map
// CHECK: </OPTIONS_BLOCK>

// CHECK-LABEL: <INPUT_BLOCK
// CHECK: <SEARCH_PATH abbrevid={{[0-9]+}} op0=1 op1=0/> blob data = 'REMAPPED/Frameworks'
// CHECK: <SEARCH_PATH abbrevid={{[0-9]+}} op0=1 op1=1/> blob data = 'REMAPPED/SystemFrameworks'
// CHECK: <SEARCH_PATH abbrevid={{[0-9]+}} op0=0 op1=0/> blob data = 'REMAPPED/secret'
// CHECK: </INPUT_BLOCK>
