// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -emit-objc-header-path %t/textual-imports.h -Xcc -fmodule-map-file=%S/Inputs/custom-modules/module.modulemap -Xcc -I%S/Inputs/custom-modules/header_subdirectory/ -I%S/Inputs/custom-modules/ -emit-clang-header-nonmodular-includes %s
// RUN: %FileCheck %s < %t/textual-imports.h

// The module map does not lie in a provided include dir, but we should still include <header-regular.h> and NOT <header_subdirectory/header-regular.h> because
// of the provided include dir pointing into header_subdirectory.

import EmitClangHeaderNonmodularIncludesStressTest

public class Bar : Baz {}

// CHECK:      #if __has_feature(objc_modules)
// CHECK-NEXT: #if __has_warning("-Watimport-in-framework-header")
// CHECK-NEXT: #pragma clang diagnostic ignored "-Watimport-in-framework-header"
// CHECK-NEXT: #endif
// CHECK-NEXT: @import EmitClangHeaderNonmodularIncludesStressTest;
// CHECK-NEXT: #else
// CHECK: #import <header-regular.h>
// CHECK: #endif
