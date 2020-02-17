// RUN: %empty-directory(%t.mcp)

// This should fail only if -disable-clangimporter-source-import is present.

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:    -enable-objc-interop -typecheck -I %S/Inputs/custom-modules \
// RUN:    -module-cache-path %t.mcp %s | %FileCheck --allow-empty %s

// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:    -enable-objc-interop -typecheck -o - -I %S/Inputs/custom-modules \
// RUN:    -module-cache-path %t.mcp -disable-clangimporter-source-import %s \
// RUN:    2>&1 | %FileCheck --check-prefix=ERROR %s

import ExternIntX

public let y = x // ERROR: error

// CHECK-NOT: error
