// RUN: %target-swift-ide-test -print-module -module-to-print=ImportCCharTypes -I %S/Inputs -source-filename=x | %FileCheck %s

// CHECK: var a_char: CChar
// CHECK: var a_wchar: wchar_t
