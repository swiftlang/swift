// RUN: %swift -primary-file %s %S/../Inputs/empty.swift -target x86_64-apple-macosx10.9 -emit-ir -g -module-name multi | FileCheck %s
// RUN: %swift %S/../Inputs/empty.swift -primary-file %s -target x86_64-apple-macosx10.9 -emit-ir -g -module-name multi | FileCheck %s
// RUN: %swift -primary-file %S/../Inputs/empty.swift %s -target x86_64-apple-macosx10.9 -emit-ir -g -module-name multi | FileCheck %s --check-prefix=CHECK-OTHER
// RUN: %swift %s -primary-file %S/../Inputs/empty.swift -target x86_64-apple-macosx10.9 -emit-ir -g -module-name multi | FileCheck %s --check-prefix=CHECK-OTHER
// XFAIL: linux
// CHECK: [ DW_TAG_compile_unit ] [{{.*}}multi-file.swift]
// CHECK-OTHER: [ DW_TAG_compile_unit ] [{{.*}}empty.swift]
