// RUN: rm -f %t.*
// RUN: %target-swift-frontend -emit-sil %s %S/Inputs/printer_include_decls_module_helper.swift -o %t.sil -module-name main
// RUN: %FileCheck --input-file=%t.sil %s
// RUN: %FileCheck --input-file=%t.sil %S/Inputs/printer_include_decls_module_helper.swift

// RUN: %target-swift-frontend -emit-sil -primary-file %s %S/Inputs/printer_include_decls_module_helper.swift -o %t.sil -module-name main
// RUN: %FileCheck --input-file=%t.sil %s
// RUN: %FileCheck --input-file=%t.sil %S/Inputs/printer_include_decls_module_helper.swift -check-prefix=CHECK-NEGATIVE

class Foo {
// CHECK: class Foo
// CHECK-NEGATIVE-NOT: class Foo
}

