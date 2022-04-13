// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -print-module -module-to-print=std -source-filename=x -enable-experimental-cxx-interop -module-cache-path %t > %t/interface.swift
// RUN: %FileCheck %s -check-prefix=CHECK-STD < %t/interface.swift
// RUN: %FileCheck %s -check-prefix=CHECK-TO-STRING < %t/interface.swift
// RUN: %FileCheck %s -check-prefix=CHECK-STRING < %t/interface.swift

// Running this test with different versions of libstdc++ will result in the decls being printed in different order.

// This test is specific to libstdc++ and only runs on platforms where libstdc++ is used.
// REQUIRES: OS=linux-gnu

// REQUIRES: rdar91670704

// CHECK-STD: enum std {
// CHECK-STD:   enum __cxx11 {
// CHECK-STD:     struct __CxxTemplateInstNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE {
// CHECK-STD:       typealias value_type = std.__CxxTemplateInstSt11char_traitsIcE.char_type
// CHECK-STD:     }
// CHECK-STD:     struct __CxxTemplateInstNSt7__cxx1112basic_stringIwSt11char_traitsIwESaIwEEE {
// CHECK-STD:       typealias value_type = std.__CxxTemplateInstSt11char_traitsIwE.char_type
// CHECK-STD:     }
// CHECK-STD:   }

// CHECK-TO-STRING:   static func to_string(_ __val: Int32) -> std{{(.__cxx11)?}}.string
// CHECK-TO-STRING:   static func to_wstring(_ __val: Int32) -> std{{(.__cxx11)?}}.wstring

// CHECK-STD:   typealias size_t = Int

// CHECK-STRING:   typealias string = std.__cxx11.__CxxTemplateInstNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE
// CHECK-STRING:   typealias wstring = std.__cxx11.__CxxTemplateInstNSt7__cxx1112basic_stringIwSt11char_traitsIwESaIwEEE
// CHECK-STD: }
