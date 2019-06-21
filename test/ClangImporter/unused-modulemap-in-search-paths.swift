// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %s -I %S/Inputs/unused-modulemap -module-cache-path %t/MCP
// RUN: llvm-bcanalyzer -dump %t/MCP/module*.swiftmodule | not grep UnusedModule

import module
