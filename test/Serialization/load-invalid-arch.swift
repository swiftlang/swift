
// RUN: %empty-directory(%t)
// RUN: mkdir %t/new_module.swiftmodule
// RUN: touch %t/new_module.swiftmodule/i387.swiftmodule
// RUN: touch %t/new_module.swiftmodule/ppc65.swiftmodule
// RUN: touch %t/new_module.swiftmodule/i387.swiftdoc
// RUN: touch %t/new_module.swiftmodule/ppc65.swiftdoc
// RUN: not %target-swift-frontend %s -typecheck -I %t -show-diagnostics-after-fatal 2>&1 | %FileCheck %s -DTARGET_ARCHITECTURE=$(echo %target-swiftmodule-name | cut -d. -f1)

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/new_module.framework/Modules/new_module.swiftmodule/
// RUN: touch %t/new_module.framework/Modules/new_module.swiftmodule/i387.swiftmodule
// RUN: touch %t/new_module.framework/Modules/new_module.swiftmodule/ppc65.swiftmodule
// RUN: not %target-swift-frontend %s -F %t -typecheck -show-diagnostics-after-fatal 2>&1 | %FileCheck %s -DTARGET_ARCHITECTURE=$(echo %target-swiftmodule-name | cut -d. -f1)

//CHECK: {{.*}} error: could not find module 'new_module' for architecture '[[TARGET_ARCHITECTURE]]'; found: {{ppc65, i387|i387, ppc65}}
//CHECK-NEXT: import new_module
//CHECK-NEXT: 		^
//CHECK: error: no such module 'new_module'
//CHECK-NEXT: import new_module
//CHECK-NEXT: 		^
//CHECK: error: use of unresolved identifier 'new_module'
//CHECK-NEXT: new_module.foo()
//CHECK-NEXT: ^~~~~~~~~~

import new_module

new_module.foo()
