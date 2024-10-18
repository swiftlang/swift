// Tests the `-playground-option SelectiveTransform` flag and the
// `@_PlaygroundTransformed` function annotation in a Swift declaration macro.
//
// REQUIRES: executable_test
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/plugins)
//
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/plugins/%target-library-name(TestMacro1Macros) -module-name=TestMacro1Macros %S/Inputs/TestMacro1Definition.swift -g -no-toolchain-stdlib-rpath
//
// XRUN: %target-build-swift -swift-version 5 -emit-module -o %t/TestMacro1.swiftmodule %S/Inputs/TestMacro1Freestanding.swift -module-name TestMacro1 -enable-library-evolution -emit-module-interface -load-plugin-library %t/plugins/%target-library-name(TestMacro1Macros)
//
// XRUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift
//
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -playground-option -Xfrontend SelectiveTransform -load-plugin-library %t/plugins/%target-library-name(TestMacro1Macros) -o %t/main -I=%t %t/PlaygroundSupport.o %t/main.swift %S/Inputs/TestMacro1.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// Calls the function in Inputs/TestMacro1.swoft
macroTesting()

// CHECK:      {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: f()
// CHECK-NEXT: {{.*}} __builtin_postPrint
// CHECK-NEXT: {{.*}} __builtin_log[='[1, 2, 3, 4]']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
