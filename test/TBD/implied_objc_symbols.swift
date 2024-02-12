// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)

// RUN: echo "import Foundation" > %t/main.swift
// RUN: echo "@objc(CApi) public class Api {}" >> %t/main.swift
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -import-objc-header %S/Inputs/objc_class_header.h -validate-tbd-against-ir=missing %t/main.swift -disable-objc-attr-requires-foundation-module -emit-tbd -emit-tbd-path %t/main.tbd -tbd-install_name objc_classes

// RUN: %validate-json %t/main.tbd |  %FileCheck %s

// CHECK-NOT: '_OBJC_CLASS_$_CApi'
// CHECK-NOT: '_OBJC_METACLASS_$_CApi' 

// CHECK: "objc_class": [
// CHECK-NEXT:   "CApi"
// CHECK-NEXT: ]

