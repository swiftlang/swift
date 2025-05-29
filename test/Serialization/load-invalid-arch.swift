
// RUN: %empty-directory(%t)
// RUN: mkdir %t/new_module.swiftmodule
// RUN: touch %t/new_module.swiftmodule/i387.swiftmodule
// RUN: touch %t/new_module.swiftmodule/ppc65.swiftmodule
// RUN: touch %t/new_module.swiftmodule/i387.swiftdoc
// RUN: touch %t/new_module.swiftmodule/ppc65.swiftdoc
// RUN: not %target-swift-frontend %s -typecheck -I %t -show-diagnostics-after-fatal 2>&1 | %FileCheck %s -check-prefix=CHECK -check-prefix CHECK-ALL -DTARGET_ARCHITECTURE=%module-target-triple

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/new_module.framework/Modules/new_module.swiftmodule/
// RUN: touch %t/new_module.framework/Modules/new_module.swiftmodule/i387.swiftmodule
// RUN: touch %t/new_module.framework/Modules/new_module.swiftmodule/ppc65.swiftmodule
// RUN: not %target-swift-frontend %s -F %t -typecheck -show-diagnostics-after-fatal 2>&1 | %FileCheck %s -check-prefix=CHECK -check-prefix CHECK-ALL -DTARGET_ARCHITECTURE=%module-target-triple

// RUN: %empty-directory(%t)
// RUN: mkdir %t/new_module.swiftmodule
// RUN: not %target-swift-frontend %s -typecheck -I %t -show-diagnostics-after-fatal 2>&1 | %FileCheck %s -check-prefix CHECK-ALL -DTARGET_ARCHITECTURE=%target

// CHECK-ALL-NOT: error:
// CHECK: {{.*}} error: could not find module 'new_module' for target '[[TARGET_ARCHITECTURE]]'; found: {{ppc65, i387|i387, ppc65}}, at: {{.*}}new_module.swiftmodule
// CHECK: import new_module
// CHECK-ALL: error: no such module 'new_module'
// CHECK-ALL: import new_module
// CHECK-ALL: error: cannot find 'new_module' in scope
// CHECK-ALL: new_module.foo()

import new_module

new_module.foo()
