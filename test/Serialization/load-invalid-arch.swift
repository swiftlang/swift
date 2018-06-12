
// RUN: %empty-directory(%t)
// RUN: mkdir %t/new_module.swiftmodule
// RUN: touch %t/new_module.swiftmodule/arm64.swiftmodule
// RUN: touch %t/new_module.swiftmodule/powerpc64.swiftmodule
// RUN: touch %t/new_module.swiftmodule/arm64.swiftdoc
// RUN: touch %t/new_module.swiftmodule/powerpc64.swiftdoc
// RUN: %target-swift-frontend %s -typecheck -I %t -verify -show-diagnostics-after-fatal

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/new_module.framework/Modules/new_module.swiftmodule/
// RUN: touch %t/new_module.framework/Modules/new_module.swiftmodule/powerpc64.swiftmodule
// RUN: touch %t/new_module.framework/Modules/new_module.swiftmodule/arm64.swiftmodule
// RUN: %target-swift-frontend %s -F %t -typecheck -verify -show-diagnostics-after-fatal

import new_module // expected-error {{no such module 'new_module'}} expected-error {{could not find module 'new_module' for architecture 'x86_64' found: powerpc64, arm64}}

new_module.foo() // expected-error {{use of unresolved identifier 'new_module'}}
