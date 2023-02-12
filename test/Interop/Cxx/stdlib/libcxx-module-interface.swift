// RUN: %target-swift-ide-test -print-module -module-to-print=CxxStdlib -source-filename=x -enable-experimental-cxx-interop -tools-directory=%llvm_obj_root/bin -module-cache-path %t | %FileCheck %s  -check-prefix=CHECK-STD
// RUN: %target-swift-ide-test -print-module -module-to-print=CxxStdlib.iosfwd -source-filename=x -enable-experimental-cxx-interop -tools-directory=%llvm_obj_root/bin -module-cache-path %t | %FileCheck %s  -check-prefix=CHECK-IOSFWD
// RUN: %target-swift-ide-test -print-module -module-to-print=CxxStdlib.string -source-filename=x -enable-experimental-cxx-interop -tools-directory=%llvm_obj_root/bin -module-cache-path %t | %FileCheck %s  -check-prefix=CHECK-STRING

// This test is specific to libc++ and therefore only runs on Darwin platforms.
// REQUIRES: OS=macosx || OS=ios

// CHECK-STD: import CxxStdlib.iosfwd
// CHECK-STD: import CxxStdlib.string

// CHECK-IOSFWD: enum std {
// CHECK-IOSFWD:   enum __1 {
// CHECK-IOSFWD:     struct basic_string<Int8, char_traits<Int8>, allocator<Int8>> : CxxRandomAccessCollection {
// CHECK-IOSFWD:       typealias value_type = CChar
// CHECK-IOSFWD:     }
// CHECK-IOSFWD:     struct basic_string<Scalar, char_traits<Scalar>, allocator<Scalar>> : CxxRandomAccessCollection {
// CHECK-IOSFWD:       typealias value_type = CWideChar
// CHECK-IOSFWD:     }
// CHECK-IOSFWD:     typealias string = std.__1.basic_string<Int8, char_traits<Int8>, allocator<Int8>>
// CHECK-IOSFWD:     typealias wstring = std.__1.basic_string<Scalar, char_traits<Scalar>, allocator<Scalar>>
// CHECK-IOSFWD:   }
// CHECK-IOSFWD: }

// CHECK-STRING: enum std {
// CHECK-STRING:   enum __1 {
// CHECK-STRING:     static func to_string(_ __val: Int32) -> std.__1.string
// CHECK-STRING:     static func to_wstring(_ __val: Int32) -> std.__1.wstring
// CHECK-STRING:   }
// CHECK-STRING: }
