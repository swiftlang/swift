// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/tmp
// RUN: %target-swiftc_driver -emit-module -disable-bridging-pch -import-objc-header %S/Inputs/app-bridging-header-to-pch.h -module-name App -emit-module-path %t/App.swiftmodule %S/Inputs/app-that-uses-pch-bridging-header.swift
// RUN: llvm-bcanalyzer -dump %t/App.swiftmodule | %FileCheck %s
// CHECK: IMPORTED_HEADER{{.*}}Inputs/app-bridging-header-to-pch.h

// Should get no warnings when we PCH-in the chained unit-test bridging header (thereby suppressing implicit import)
// RUN: %target-swiftc_driver -D UNIT_TESTS -typecheck -Xfrontend -verify -enable-bridging-pch -import-objc-header %S/Inputs/chained-unit-test-bridging-header-to-pch.h  -I %S/Inputs -I %t %s

// Should get no warnings when we PCH-in the app bridging header (thereby suppressing implicit import)
// RUN: %target-swiftc_driver -typecheck -Xfrontend -verify -enable-bridging-pch -import-objc-header %S/Inputs/app-bridging-header-to-pch.h -I %t %s

import App

func test_all() {
#if UNIT_TESTS
  let _ = unit_test_function(AppFunc())
#else
  let _ = app_function(AppFunc())
#endif
}
