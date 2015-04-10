// RUN: rm -rf %t && mkdir -p %t/secret
// RUN: %target-swift-frontend -emit-module -o %t/secret %S/Inputs/struct_with_operators.swift
// RUN: mkdir -p %t/Frameworks/has_alias.framework/Modules/has_alias.swiftmodule/
// RUN: %target-swift-frontend -emit-module -o %t/Frameworks/has_alias.framework/Modules/has_alias.swiftmodule/%target-swiftmodule-name %S/Inputs/alias.swift -module-name has_alias

// RUN: %target-swift-frontend -emit-module -o %t -I %t/secret -F %t/Frameworks -parse-as-library %S/Inputs/has_xref.swift
// RUN: %target-swift-frontend %s -parse -I %t -verify -show-diagnostics-after-fatal

// Try again, treating has_xref as a main file to force serialization to occur.
// RUN: %target-swift-frontend -emit-module -o %t -I %t/secret -F %t/Frameworks %S/Inputs/has_xref.swift
// RUN: %target-swift-frontend %s -parse -I %t

// RUN: %target-swift-frontend -emit-module -o %t -I %t/secret -F %t/Frameworks -parse-as-library %S/Inputs/has_xref.swift -serialize-debugging-options
// RUN: %target-swift-frontend %s -parse -I %t

// RUN: %target-swift-frontend -emit-module -o %t -I %t/secret -F %t/Frameworks -parse-as-library %S/Inputs/has_xref.swift -application-extension
// RUN: %target-swift-frontend %s -parse -I %t

// Make sure we don't end up with duplicate search paths.
// RUN: %target-swiftc_driver -emit-module -o %t/has_xref.swiftmodule -I %t/secret -F %t/Frameworks -parse-as-library %S/Inputs/has_xref.swift %S/../Inputs/empty.swift -Xfrontend -serialize-debugging-options
// RUN: %target-swift-frontend %s -parse -I %t
// RUN: llvm-bcanalyzer -dump %t/has_xref.swiftmodule | FileCheck %s

// RUN: %target-swift-frontend %s -emit-module -o %t/main.swiftmodule -I %t -I %t/secret -F %t/Frameworks
// RUN: llvm-bcanalyzer -dump %t/main.swiftmodule | FileCheck %s

// XFAIL: linux

import has_xref // expected-error {{missing required modules: 'has_alias', 'struct_with_operators'}}

numeric(42) // expected-error {{use of unresolved identifier 'numeric'}}

// CHECK: <INPUT_BLOCK
// CHECK-NOT: /secret'
// CHECK-NOT: /Frameworks'
// CHECK: <SEARCH_PATH abbrevid={{[0-9]+}} op0=0/> blob data = '{{.+}}/secret'
// CHECK-NOT: /secret'
// CHECK: <SEARCH_PATH abbrevid={{[0-9]+}} op0=1/> blob data = '{{.+}}/Frameworks'
// CHECK-NOT: /secret'
// CHECK-NOT: /Frameworks'
// CHECK: </INPUT_BLOCK>
