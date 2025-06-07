// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -print-module -module-to-print CxxStdlib -source-filename none -enable-experimental-cxx-interop -module-cache-path %t | %FileCheck %s -check-prefix CHECK-STD
// RUN: %target-swift-ide-test -print-module -module-to-print=CxxStdlib.string -source-filename=x -enable-experimental-cxx-interop -module-cache-path %t | %FileCheck %s -check-prefix CHECK-STRING

// This test is specific to msvcprt and therefore only runs on Windows.
// REQUIRES: OS=windows-msvc

// CHECK-STD: import CxxStdlib.iosfwd
// CHECK-STD: import CxxStdlib.string

// CHECK-STRING: enum std {
// CHECK-STRING:   typealias size_t = size_t
// CHECK-STRING:   static func to_string(_ _Val: Int32) -> std.string
// CHECK-STRING:   static func to_wstring(_ _Val: Int32) -> std.wstring
// CHECK-STRING:   struct basic_string<CChar, std.char_traits<CChar>, std.allocator<CChar>> : CxxRandomAccessCollection {
// CHECK-STRING:     typealias value_type = CChar
// CHECK-STRING:   }
// CHECK-STRING:   struct basic_string<CWideChar, std.char_traits<CWideChar>, std.allocator<CWideChar>> : CxxRandomAccessCollection {
// CHECK-STRING:     typealias value_type = CWideChar
// CHECK-STRING:   }
// CHECK-STRING:   typealias string = std.basic_string<CChar, std.char_traits<CChar>, std.allocator<CChar>>
// CHECK-STRING:   typealias wstring = std.basic_string<CWideChar, std.char_traits<CWideChar>, std.allocator<CWideChar>>
// CHECK-STRING: }

