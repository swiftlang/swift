// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)

// RUN: echo "import Foundation" > %t/main.swift
// RUN: echo "@objc public class ObjCOnly {}" >> %t/main.swift
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -import-objc-header %S/Inputs/objc_class_header.h -validate-tbd-against-ir=missing %t/main.swift -disable-objc-attr-requires-foundation-module -emit-tbd -emit-tbd-path %t/main.tbd

// RUN: %FileCheck %s < %t/main.tbd

// CHECK: __TtC4test8ObjCOnly
// CHECK-NOT: _OBJC_CLASS_
// CHECK-NOT: _OBJC_METACLASS_
