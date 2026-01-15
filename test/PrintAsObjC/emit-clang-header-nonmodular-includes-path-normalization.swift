// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: cd %S/Inputs/custom-modules
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -emit-objc-header-path %t/textual-imports.h -Xcc -fmodule-map-file=%S/Inputs/custom-modules/module.modulemap -Xcc -I%S/Inputs/custom-modules/./header_subdirectory/ -Xcc -fmodule-map-file=%S/Inputs/custom-modules/header_above_modulemap/subdir/module.modulemap -Xcc -I. -emit-clang-header-nonmodular-includes %s
// RUN: %FileCheck %s < %t/textual-imports.h

// The period in the provided include directory above should not break the system.
import EmitClangHeaderNonmodularIncludesStressTest
import EmitClangHeaderWithParentDir

public class Bar : Baz {}

public class Sub : Parent {}

// CHECK:      #if __has_feature(objc_modules)
// CHECK:      #if __has_feature(objc_modules)
// CHECK-NEXT: #if __has_warning("-Watimport-in-framework-header")
// CHECK-NEXT: #pragma clang diagnostic ignored "-Watimport-in-framework-header"
// CHECK-NEXT: #endif
// CHECK-NEXT: @import EmitClangHeaderNonmodularIncludesStressTest;
// CHECK-NEXT: @import EmitClangHeaderWithParentDir;
// CHECK-NEXT: #elif defined(__OBJC__)
// CHECK-NEXT: #import <header-regular.h>
// CHECK-NEXT: #import <header-symlink.h>
// CHECK-NEXT: #import <header_above_modulemap/header-above-modulemap.h>
// CHECK-NEXT: #else
// CHECK-NEXT: #include <header-regular.h>
// CHECK-NEXT: #include <header-symlink.h>
// CHECK-NEXT: #include <header_above_modulemap/header-above-modulemap.h>
// CHECK-NEXT: #endif
