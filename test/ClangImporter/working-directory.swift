// RUN: %empty-directory(%t/mcp)

// Check that equivalent invocations result in the same module hash
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -Xcc -I -Xcc custom-modules -module-cache-path %t/mcp -Xcc -working-directory -Xcc %S/Inputs/  %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -Xcc -I -Xcc %S/Inputs/custom-modules -module-cache-path %t/mcp %s
// RUN: find %t/mcp -name "ObjCParseExtras-*.pcm" | count 1

// Check that the old working directory argument is mapped to the new format
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -Xcc -I -Xcc custom-modules %s -dump-clang-diagnostics -module-cache-path %t/mcp -Xcc -working-directory%S/Inputs 2>&1 | %FileCheck %s

// Check that the working directory is set to the CWD if not explicitly passed
// RUN: cd %S/Inputs/ && %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -Xcc -I -Xcc custom-modules %s -dump-clang-diagnostics -module-cache-path %t/mcp 2>&1 | %FileCheck %s

// REQUIRES: objc_interop

import ObjCParseExtras

print(SomeImageName)

// CHECK: clang
// CHECK: '-working-directory' 'SOURCE_DIR/test/ClangImporter/Inputs'
