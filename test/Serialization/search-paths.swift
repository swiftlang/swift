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

import has_xref // expected-error {{missing required modules: 'has_alias', 'struct_with_operators'}}

numeric(42) // expected-error {{use of unresolved identifier 'numeric'}}
