// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -function-sections -emit-library -emit-ir -static -parse-stdlib %S/Inputs/FunctionSections.swift | %FileCheck %s

// CHECK: define {{(dllexport |protected )?}}swiftcc void @"$s16FunctionSections5func1yyF"() #0 {
// CHECK: entry:
// CHECK:   ret void
// CHECK: }

// CHECK: define {{(dllexport |protected )?}}swiftcc void @"$s16FunctionSections5func2yyF"() #0 {
// CHECK: entry:
// CHECK:   ret void
// CHECK: }
