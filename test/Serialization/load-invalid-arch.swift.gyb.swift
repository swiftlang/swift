
// RUN: %empty-directory(%t)
// RUN: mkdir %t/new_module.swiftmodule
// RUN: touch %t/new_module.swiftmodule/i387.swiftmodule
// RUN: touch %t/new_module.swiftmodule/ppc65.swiftmodule
// RUN: touch %t/new_module.swiftmodule/i387.swiftdoc
// RUN: touch %t/new_module.swiftmodule/ppc65.swiftdoc
// RUN: %gyb -DTARGET_ARCHITECTURE=%target-cpu %s -o %t/load-invalid-arc.swift
// RUN: %target-swift-frontend %t/load-invalid-arc.swift -typecheck -I %t -verify -show-diagnostics-after-fatal

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/new_module.framework/Modules/new_module.swiftmodule/
// RUN: touch %t/new_module.framework/Modules/new_module.swiftmodule/i387.swiftmodule
// RUN: touch %t/new_module.framework/Modules/new_module.swiftmodule/ppc65.swiftmodule
// RUN: %gyb -DTARGET_ARCHITECTURE=%target-cpu %s -o %t/load-invalid-arc.swift
// RUN: %target-swift-frontend %t/load-invalid-arc.swift -F %t -typecheck -verify -show-diagnostics-after-fatal

import new_module // expected-error {{no such module 'new_module'}} expected-error {{could not find module 'new_module' for architecture '${TARGET_ARCHITECTURE}'; found: ppc65, i387}}

new_module.foo() // expected-error {{use of unresolved identifier 'new_module'}}
