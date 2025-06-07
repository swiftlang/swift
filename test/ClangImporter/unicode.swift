// RUN: %swift-ide-test -target x86_64-unknown-windows-msvc -print-module -module-to-print unicode -print-interface -source-filename %s -I %S/Inputs/unicode | %FileCheck %s -check-prefix CHECK-C
// RUN: %swift-ide-test -target x86_64-unknown-windows-msvc -enable-experimental-cxx-interop -print-module -module-to-print unicode -print-interface -source-filename %s -I %S/Inputs/unicode | %FileCheck %s -check-prefix CHECK-CXX

// REQUIRES: OS=windows-msvc

import unicode

// CHECK-C: typealias WCHAR = wchar_t
// CHECK-CXX: typealias WCHAR = CWideChar
// CHECK: var UNICODE_NULL: WCHAR { get }
