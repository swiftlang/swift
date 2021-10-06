// RUN: %target-swift-ide-test -print-module -module-to-print=std -source-filename=x -enable-cxx-interop -tools-directory=%llvm_obj_root/bin -module-cache-path %t | %FileCheck %s  -check-prefix=CHECK-STD
// RUN: %target-swift-ide-test -print-module -module-to-print=std.iosfwd -source-filename=x -enable-cxx-interop -tools-directory=%llvm_obj_root/bin -module-cache-path %t | %FileCheck %s  -check-prefix=CHECK-IOSFWD
// RUN: %target-swift-ide-test -print-module -module-to-print=std.string -source-filename=x -enable-cxx-interop -tools-directory=%llvm_obj_root/bin -module-cache-path %t | %FileCheck %s  -check-prefix=CHECK-STRING

// Clang driver on Windows doesn't support -stdlib=libc++
// XFAIL: OS=windows-msvc

// libstdc++ cannot currently be imported.
// XFAIL: OS=linux-gnu

// XFAIL: OS=linux-androideabi
// XFAIL: OS=linux-android

// CHECK-STD: import std.iosfwd
// CHECK-STD: import std.string

// CHECK-IOSFWD: extension std.__1 {
// CHECK-IOSFWD:   struct __CxxTemplateInstNSt3__112basic_stringIcNS_11char_traitsIcEENS_9allocatorIcEEEE {
// CHECK-IOSFWD:     typealias value_type = CChar
// CHECK-IOSFWD:   }
// CHECK-IOSFWD:   struct __CxxTemplateInstNSt3__112basic_stringIwNS_11char_traitsIwEENS_9allocatorIwEEEE {
// CHECK-IOSFWD:     typealias value_type = CWideChar
// CHECK-IOSFWD:   }
// CHECK-IOSFWD:   typealias string = std.__1.__CxxTemplateInstNSt3__112basic_stringIcNS_11char_traitsIcEENS_9allocatorIcEEEE
// CHECK-IOSFWD:   typealias wstring = std.__1.__CxxTemplateInstNSt3__112basic_stringIwNS_11char_traitsIwEENS_9allocatorIwEEEE
// CHECK-IOSFWD: }

// CHECK-STRING: extension std.__1 {
// CHECK-STRING:   static func to_string(_ __val: Int32) -> std.__1.string
// CHECK-STRING:   static func to_wstring(_ __val: Int32) -> std.__1.wstring
// CHECK-STRING: }

// CHECK-IOSFWD-NOT: static func to_string
// CHECK-STRING-NOT: typealias string
// CHECK-STD-NOT: extension std
