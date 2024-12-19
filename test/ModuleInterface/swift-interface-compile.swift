// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-cache-path %t/mcp -I %S/Inputs/SR77756 -c %s -o /dev/null -D STATIC
// RUN: llvm-bcanalyzer -dump %t/mcp/static-*.swiftmodule | %FileCheck %s -check-prefix CHECK-STATIC
// RUN: %target-swift-frontend -module-cache-path %t/mcp -I %S/Inputs/SR77756 -c %s -o /dev/null
// RUN: llvm-bcanalyzer -dump %t/mcp/dynamic-*.swiftmodule | %FileCheck %s -check-prefix CHECK-DYNAMIC

#if STATIC
import `static`
#else
import `dynamic`
#endif

// CHECK-STATIC: IS_STATIC
// CHECK-DYNAMIC-NOT: IS_STATIC
