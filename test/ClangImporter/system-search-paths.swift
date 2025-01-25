// The clang modules trigger a warning, make sure that -Fsystem/-Isystem have the effect of importing as system, which will suppress the warning.

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/systemsearchpaths/Frameworks -I %S/Inputs/systemsearchpaths/include -module-cache-path %t/mcp1 %s 2> %t/stderr-as-user.txt
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -Fsystem %S/Inputs/systemsearchpaths/Frameworks -Isystem %S/Inputs/systemsearchpaths/include -module-cache-path %t/mcp2 %s 2> %t/stderr-as-system.txt
// RUN: %FileCheck -input-file=%t/stderr-as-user.txt %s -check-prefix=CHECK-USER
// RUN: %FileCheck -input-file=%t/stderr-as-system.txt %s -check-prefix=CHECK-SYSTEM --allow-empty

// CHECK-USER-COUNT-2: non-void function does not return a value
// CHECK-SYSTEM-NOT: non-void function does not return a value
import FWModule
import Module
