// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/tmp
// RUN: %target-swiftc_driver -emit-module -disable-bridging-pch -import-objc-header %S/Inputs/app-bridging-header-to-pch.h -module-name App -emit-module-path %t/App.swiftmodule %S/Inputs/app-that-uses-pch-bridging-header.swift
// RUN: llvm-bcanalyzer -dump %t/App.swiftmodule | %FileCheck %s
// CHECK: IMPORTED_HEADER{{.*}}Inputs/app-bridging-header-to-pch.h

// Should get a warning when we PCH-in the unit test header and then implicitly import the app header.
// RUN: %target-swiftc_driver -D UNIT_TESTS -typecheck -Xfrontend -verify -enable-bridging-pch -import-objc-header %S/Inputs/unit-test-bridging-header-to-pch.h -I %t %s

import App // expected-warning{{implicit import of bridging header 'app-bridging-header-to-pch.h' via module 'App' is deprecated and will be removed in a later version of Swift}}

func test_all() {
#if UNIT_TESTS
  let _ = unit_test_function(AppFunc())
#else
  let _ = app_function(AppFunc())
#endif
}
