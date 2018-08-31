// RUN: %empty-directory(%t)

// RUN: cp %S/Inputs/serialized-objc-header.h %t
// RUN: %target-build-swift -emit-executable %S/ASTSection.swift -g -o %t/ASTSection-with-ObjC -import-objc-header %t/serialized-objc-header.h -DOBJC -module-name ASTSection -emit-module
// RUN: %lldb-moduleimport-test -verbose %t/ASTSection-with-ObjC | %FileCheck %s

// RUN: rm %t/serialized-objc-header.h
// RUN: %lldb-moduleimport-test -verbose %t/ASTSection-with-ObjC | %FileCheck %s

// RUN: %target-build-swift -emit-executable %S/ASTSection.swift -gline-tables-only -o %t/ASTSection -emit-module
// RUN: %lldb-moduleimport-test -verbose %t/ASTSection | %FileCheck %s --allow-empty --check-prefix=LINETABLE-CHECK

// REQUIRES: executable_test
// REQUIRES: objc_interop

// CHECK: - Target: {{.+}}-{{.+}}-{{.+}}
// CHECK: Importing ASTSection... ok!

// LINETABLE-CHECK-NOT: ASTSection
