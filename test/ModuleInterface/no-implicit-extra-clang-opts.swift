// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ModuleCache)

// https://github.com/apple/swift/issues/70330
// We can't yet call member functions correctly on Windows.
// XFAIL: OS=windows-msvc

import SIMod

// Step 0: Copy relevant files into the temp dir which will serve as the search path
// RUN: cp %S/Inputs/implicit-options-inheritance/module.modulemap %t/module.modulemap
// RUN: cp %S/Inputs/implicit-options-inheritance/test-dummy.modulemap %t/test-dummy.modulemap
// RUN: cp %S/Inputs/implicit-options-inheritance/CIMod.h %t/CIMod.h
// RUN: cp %S/Inputs/implicit-options-inheritance/SIMod.swiftinterface %t/SIMod.swiftinterface

// Step 1: Build this file, causing an implicit build of SIMod and CIMod into the module cache.
// Pass in a clang arg pointing it to a modulemap that has nothing to do with downstream modules and is not on the search path.

// RUN: %target-swift-frontend -emit-module -module-name no-implicit-extra-clang-maps -o %t/no-implicit-extra-clang-maps.swiftmodule %s -I %t -Xcc -fmodule-map-file=%t/test-dummy.modulemap -module-cache-path %t/ModuleCache

// Step 2: Touch the dummy modulemap we passed in with `-Xcc -fmodule-map-file` above.
// RUN: touch %t/test-dummy.modulemap

// Step 3: Re-build this file, and ensure we are not re-building SIMod due to a dependency on the dummy file
// RUN: %target-swift-frontend -emit-module -module-name no-implicit-extra-clang-maps -o %t/no-implicit-extra-clang-maps.swiftmodule %s -I %t -Xcc -fmodule-map-file=%t/test-dummy.modulemap -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild 2>&1 | %FileCheck -allow-empty %s

// Step 4: Ensure that SIMod was not re-built
// CHECK-NOT: remark: rebuilding module 'SIMod' from interface
// CHECK-NOT: note: cached module is out of date
// CHECK-NOT: note: dependency is out of date
