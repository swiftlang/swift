// RUN: rm -rf %t
// RUN: mkdir %t

// RUN: %target-swift-frontend -emit-module -o %t %S/../Inputs/empty.swift -module-name new_module
// RUN: %target-swift-frontend -emit-module -o %t %S/../Inputs/empty.swift -module-name another_new_module
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/depends_on_new_module.swift -I %t
// RUN: %target-swift-frontend %s -parse -I %t
// RUN: rm %t/new_module.swiftmodule
// RUN: %target-swift-frontend %s -parse -I %t -verify -show-diagnostics-after-fatal

// Attempt to round-trip dependency information through module merging.
// RUN: %target-swift-frontend -emit-module -o %t %S/../Inputs/empty.swift -module-name new_module
// RUN: mv %t/depends_on_new_module.swiftmodule %t/depends2.swiftmodule
// RUN: %target-swift-frontend -emit-module -parse-as-library -o %t %t/depends2.swiftmodule -module-name depends_on_new_module -I %t
// RUN: %target-swift-frontend %s -parse -I %t
// RUN: rm %t/new_module.swiftmodule
// RUN: %target-swift-frontend %s -parse -I %t -verify -show-diagnostics-after-fatal

// This error should happen after we've deleted the dependency module
import depends_on_new_module // expected-error{{missing required module 'new_module'}}

depends_on_new_module.foo() // expected-error {{module 'depends_on_new_module' has no member named 'foo'}}
