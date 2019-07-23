// The clang module triggers a warning, make sure that -Fsystem has the effect of importing as system, which will suppress the warning.

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/systemframeworks -module-cache-path %t/mcp1 %s 2> %t/stderr-as-user.txt
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -Fsystem %S/Inputs/systemframeworks -module-cache-path %t/mcp2 %s 2> %t/stderr-as-system.txt
// RUN: %FileCheck -input-file=%t/stderr-as-user.txt %s -check-prefix=CHECK-USER
// RUN: %FileCheck -input-file=%t/stderr-as-system.txt %s -check-prefix=CHECK-SYSTEM --allow-empty

// CHECK-USER: control reaches end of non-void function
// CHECK-SYSTEM-NOT: control reaches end of non-void function
import Module
