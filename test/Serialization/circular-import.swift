// Circularities involving the module currently being type-checked.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s -module-name A -o %t
// RUN: %target-swift-frontend -emit-module %s -module-name B -D IMPORT_A -I %t -o %t
// RUN: not %target-swift-frontend -typecheck %s -module-name A -D IMPORT_B -I %t 2>&1 | %FileCheck -check-prefix CHECK-ABA %s

// RUN: %target-swift-frontend -emit-module %s -module-name C -D IMPORT_B -I %t -o %t
// RUN: not %target-swift-frontend -typecheck %s -module-name A -D IMPORT_C -I %t 2>&1 | %FileCheck -check-prefix CHECK-ABCA %s

// Circularities not involving the module currently being type-checked.
// RUN: %empty-directory(%t/plain)
// RUN: %target-swift-frontend -emit-module %s -module-name A -o %t/plain
// RUN: %target-swift-frontend -emit-module %s -module-name B -o %t/plain
// RUN: %empty-directory(%t/cycle)
// RUN: %target-swift-frontend -emit-module %s -module-name A -D IMPORT_B -o %t/cycle -I %t/plain
// RUN: %target-swift-frontend -emit-module %s -module-name B -D IMPORT_A -o %t/cycle -I %t/plain
// RUN: not %target-swift-frontend -typecheck %s -module-name C -D IMPORT_A -I %t/cycle 2>&1 | %FileCheck -check-prefix CHECK-ABA-BUILT %s


#if IMPORT_A
import A
// CHECK-ABA-BUILT: <unknown>:0: error: circular dependency between modules 'A' and 'B'
#endif

#if IMPORT_B
import B
// CHECK-ABA: :[[@LINE-1]]:8: error: circular dependency between modules 'A' and 'B'
#endif

#if IMPORT_C
import C
// CHECK-ABCA: <unknown>:0: error: circular dependency between modules 'A' and 'B'
#endif
