// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -print-module -module-to-print=CxxStdlib -source-filename=x -enable-experimental-cxx-interop -module-cache-path %t > %t/interface.swift
// RUN: %FileCheck %s -check-prefix=CHECK-STD < %t/interface.swift
// RUN: %FileCheck %s -check-prefix=CHECK-SIZE-T < %t/interface.swift
// RUN: %FileCheck %s -check-prefix=CHECK-TO-STRING < %t/interface.swift
// RUN: %FileCheck %s -check-prefix=CHECK-STRING < %t/interface.swift

// Running this test with different versions of libstdc++ will result in the decls being printed in different order.

// This test is specific to libstdc++ and only runs on platforms where libstdc++ is used.
// REQUIRES: OS=linux-gnu

// CHECK-STD: enum std {
// CHECK-STRING:   struct basic_string<CChar, std{{(.__cxx11)?}}.char_traits<CChar>, std{{(.__cxx11)?}}.allocator<CChar>> : CxxMutableRandomAccessCollection {
// CHECK-STRING:     typealias value_type = std.char_traits<CChar>.char_type
// CHECK-STRING:   }
// CHECK-STRING:   struct basic_string<CWideChar, std{{(.__cxx11)?}}.char_traits<CWideChar>, std{{(.__cxx11)?}}.allocator<CWideChar>> : CxxMutableRandomAccessCollection {
// CHECK-STRING:     typealias value_type = std.char_traits<CWideChar>.char_type
// CHECK-STRING:   }

// CHECK-TO-STRING:   static func to_string(_ __val: Int32) -> std{{(.__cxx11)?}}.string
// CHECK-TO-STRING:   static func to_wstring(_ __val: Int32) -> std{{(.__cxx11)?}}.wstring

// CHECK-SIZE-T:   typealias size_t = Int

// CHECK-STRING:   typealias string =  std{{(.__cxx11)?}}.basic_string<CChar, std{{(.__cxx11)?}}.char_traits<CChar>, std{{(.__cxx11)?}}.allocator<CChar>>
// CHECK-STRING:   typealias wstring =  std{{(.__cxx11)?}}.basic_string<CWideChar, std{{(.__cxx11)?}}.char_traits<CWideChar>, std{{(.__cxx11)?}}.allocator<CWideChar>>
// CHECK-STD: }
