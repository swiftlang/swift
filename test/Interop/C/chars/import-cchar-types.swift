// RUN: %target-swift-ide-test -print-module -module-to-print=ImportCCharTypes -I %S/Inputs -source-filename=x | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=ImportCCharTypes -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default -Xcc -std=c++20 | %FileCheck %s --check-prefix=CHECK-CXX

// CHECK: var a_char: CChar
// CHECK: var a_wchar: wchar_t

// CHECK-CXX: var small_char: UInt8
