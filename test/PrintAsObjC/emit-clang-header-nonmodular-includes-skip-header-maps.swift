// REQUIRES: objc_interop

// RUN: %empty-directory(%t)

// Create a header map that maps header-regular.h to its real path.
// RUN: echo '{"mappings": {"header-regular.h": "%/S/Inputs/custom-modules/header_subdirectory/header-regular.h"}}' > %t/hmap.json
// RUN: %hmaptool write %t/hmap.json %t/headers.hmap

// Compile with the header map as a search directory alongside a regular
// include directory. The header map should be skipped when collecting include
// paths for nonmodular includes.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -emit-objc-header-path %t/textual-imports.h -Xcc -fmodule-map-file=%S/Inputs/custom-modules/module.modulemap -Xcc -I%t/headers.hmap -Xcc -I%S/Inputs/custom-modules/header_subdirectory/ -emit-clang-header-nonmodular-includes %s
// RUN: %FileCheck %s < %t/textual-imports.h

import EmitClangHeaderNonmodularIncludesStressTest

public class Bar : Baz {}

// CHECK: @import EmitClangHeaderNonmodularIncludesStressTest;
// CHECK-NEXT: #elif defined(__OBJC__)
// CHECK-NEXT: #import <header-regular.h>
// CHECK-NEXT: #import <header-symlink.h>
// CHECK-NEXT: #else
// CHECK-NEXT: #include <header-regular.h>
// CHECK-NEXT: #include <header-symlink.h>
// CHECK-NEXT: #endif
