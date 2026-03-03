// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module -disable-bridging-pch -import-objc-header %S/Inputs/pch-bridging-header-with-another-bridging-header/app.h -I %S/Inputs/pch-bridging-header-with-another-bridging-header -module-name App -emit-module-path %t/App.swiftmodule %S/../Inputs/empty.swift
// RUN: llvm-bcanalyzer -dump %t/App.swiftmodule | %FileCheck %s

// CHECK: IMPORTED_HEADER{{.*}}Inputs/pch-bridging-header-with-another-bridging-header/app.h

// Now load the app-module-with-bridging-header along with another bridging
// header that we precompile. This is going to the frontend directly to make
// sure we validate PCH inputs (because -pch-disable-validation wasn't passed).
// This is deliberately run twice to test what happens when the PCH is already
// there. (It used to crash.)

// RUN: cp %S/Inputs/pch-bridging-header-with-another-bridging-header/unit-tests.h %t/unit-tests.h
// RUN: %target-swift-frontend -typecheck -pch-output-dir %t -import-objc-header %t/unit-tests.h -I %S/Inputs/pch-bridging-header-with-another-bridging-header -I %t %s
// RUN: %target-swift-frontend -typecheck -pch-output-dir %t -import-objc-header %t/unit-tests.h -I %S/Inputs/pch-bridging-header-with-another-bridging-header -I %t %s
// RUN: echo >> %t/unit-tests.h
// RUN: %target-swift-frontend -typecheck -pch-output-dir %t -import-objc-header %t/unit-tests.h -I %S/Inputs/pch-bridging-header-with-another-bridging-header -I %t %s

import App

_ = app_function(2)
