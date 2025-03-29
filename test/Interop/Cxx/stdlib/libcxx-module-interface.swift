// Only run this test with older libc++, before the top-level std module got split into multiple top-level modules.
// RUN: %empty-directory(%t)
// RUN: %target-clangxx %S/Inputs/check-libcxx-version.cpp -o %t/check-libcxx-version
// RUN: %target-codesign %t/check-libcxx-version

// RUN: %target-run %t/check-libcxx-version || %target-swift-ide-test -print-module -module-to-print=CxxStdlib -source-filename=x -enable-experimental-cxx-interop -module-cache-path %t | %FileCheck %s  -check-prefix=CHECK-STD
// RUN: %target-run %t/check-libcxx-version || %target-swift-ide-test -print-module -module-to-print=CxxStdlib -source-filename=x -enable-experimental-cxx-interop -module-cache-path %t -module-print-submodules | %FileCheck %s  -check-prefix=CHECK-STD-WITH-SUBMODULES
// RUN: %target-run %t/check-libcxx-version || %target-swift-ide-test -print-module -module-to-print=CxxStdlib.iosfwd -source-filename=x -enable-experimental-cxx-interop -module-cache-path %t | %FileCheck %s  -check-prefix=CHECK-IOSFWD
// RUN: %target-run %t/check-libcxx-version || %target-swift-ide-test -print-module -module-to-print=CxxStdlib.string -source-filename=x -enable-experimental-cxx-interop -module-cache-path %t | %FileCheck %s  -check-prefix=CHECK-STRING

// This test is specific to libc++ and therefore only runs on Darwin platforms.
// REQUIRES: OS=macosx || OS=ios

// Since this test runs check-libcxx-version, it requires execution.
// REQUIRES: executable_test

// CHECK-STD: import CxxStdlib.iosfwd
// CHECK-STD: import CxxStdlib.string

// CHECK-STD-WITH-SUBMODULES: enum std {
// CHECK-STD-WITH-SUBMODULES: enum __1 {
// CHECK-STD-WITH-SUBMODULES-NOT: enum std

// CHECK-IOSFWD: enum std {
// CHECK-IOSFWD:   enum __1 {
// CHECK-IOSFWD:     struct basic_string<CChar, std.__1.char_traits<CChar>, std.__1.allocator<CChar>> : CxxMutableRandomAccessCollection {
// CHECK-IOSFWD:       typealias value_type = CChar
// CHECK-IOSFWD:     }
// CHECK-IOSFWD:     struct basic_string<CWideChar, std.__1.char_traits<CWideChar>, std.__1.allocator<CWideChar>> : CxxMutableRandomAccessCollection {
// CHECK-IOSFWD:       typealias value_type = CWideChar
// CHECK-IOSFWD:     }
// CHECK-IOSFWD:     typealias string = std.__1.basic_string<CChar, std.__1.char_traits<CChar>, std.__1.allocator<CChar>>
// CHECK-IOSFWD:     typealias wstring = std.__1.basic_string<CWideChar, std.__1.char_traits<CWideChar>, std.__1.allocator<CWideChar>>
// CHECK-IOSFWD:   }
// CHECK-IOSFWD: }
// CHECK-IOSFWD-NOT: enum std

// CHECK-STRING: enum std {
// CHECK-STRING:   enum __1 {
// CHECK-STRING:     static func to_string(_ __val: Int32) -> std.__1.string
// CHECK-STRING:     static func to_wstring(_ __val: Int32) -> std.__1.wstring
// CHECK-STRING:   }
// CHECK-STRING: }
// CHECK-STRING-NOT: enum std
