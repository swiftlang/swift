// RUN: cd %S/Inputs/ && %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -Xcc -I -Xcc custom-modules %s -dump-clang-diagnostics 2>&1 | %FileCheck %s

// REQUIRES: objc_interop

import ObjCParseExtras

print(SomeImageName)

// CHECK: clang
// CHECK: '-working-directory' '{{[^"]+}}/Inputs'
