// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -print-module -module-to-print std -source-filename none -enable-experimental-cxx-interop -module-cache-path %t | %FileCheck %s -check-prefix CHECK-STD
// RUN: %target-swift-ide-test -print-module -module-to-print=std.string -source-filename=x -enable-experimental-cxx-interop -module-cache-path %t | %FileCheck %s -check-prefix CHECK-STRING

// This test is specific to msvcprt and therefore only runs on Windows.
// REQUIRES: OS=windows-msvc

// CHECK-STD: import std.iosfwd
// CHECK-STD: import std.string

// CHECK-STRING: enum std {
// CHECK-STRING:   typealias size_t = size_t
// CHECK-STRING:   static func to_string(_ _Val: Int32) -> std.string
// CHECK-STRING:   static func to_wstring(_ _Val: Int32) -> std.wstring
// CHECK-STRING:   struct __CxxTemplateInstSs {
// CHECK-STRING:     typealias value_type = CChar
// CHECK-STRING:   }
// CHECK-STRING:   struct __CxxTemplateInstSbIwSt11char_traitsIwESaIwEE {
// CHECK-STRING:     typealias value_type = CWideChar
// CHECK-STRING:   }
// CHECK-STRING:   typealias string = std.__CxxTemplateInstSs
// CHECK-STRING:   typealias wstring = std.__CxxTemplateInstSbIwSt11char_traitsIwESaIwEE
// CHECK-STRING: }

