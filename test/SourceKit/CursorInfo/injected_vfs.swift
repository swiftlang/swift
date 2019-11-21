import CModule
import SwiftModule

func foo(
    _ structDefinedInCModule: StructDefinedInCModule,
    _ structDefinedInSwiftModule: StructDefinedInSwiftModule,
    _ structDefinedInSameTarget: StructDefinedInSameTarget
) {
}

// CHECK-CMODULE: key.kind: source.lang.swift.ref.struct
// CHECK-CMODULE: key.name: "StructDefinedInCModule"
// CHECK-CMODULE: key.filepath: "{{.*}}/CModule{{/|\\\\}}CModule.h"

// CHECK-SWIFTMODULE-REF: key.kind: source.lang.swift.ref.struct
// CHECK-SWIFTMODULE-REF: key.name: "StructDefinedInSwiftModule"

// CHECK-SWIFTMODULE-DECL: key.kind: source.lang.swift.decl.struct
// CHECK-SWIFTMODULE-DECL: key.name: "StructDefinedInSwiftModule"

// CHECK-SAMETARGET-REF: key.kind: source.lang.swift.ref.struct
// CHECK-SAMETARGET-REF: key.name: "StructDefinedInSameTarget"

// CHECK-SAMETARGET-DECL: key.kind: source.lang.swift.decl.struct
// CHECK-SAMETARGET-DECL: key.name: "StructDefinedInSameTarget"

// RUN: not %sourcekitd-test -req=cursor -vfs-name nope %s -dont-print-request 2>&1 | %FileCheck %s -check-prefix=NONEXISTENT_VFS_ERROR
// NONEXISTENT_VFS_ERROR: error response (Request Failed): unknown virtual filesystem 'nope'

// == Build the SwiftModule ==
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/SwiftModule.swiftmodule -module-name SwiftModule %S/../Inputs/vfs/SwiftModule/SwiftModule.swift

// == CursorInfo works for struct defined in CModule ==
// RUN: %sourcekitd-test -req=cursor -pos=5:43 -print-raw-response -vfs-files=%t/VFS/target_file1.swift=%s,%t/VFS/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %t/VFS/target_file1.swift -- %t/VFS/target_file1.swift %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple | %FileCheck --check-prefix=CHECK-CMODULE %s
// USR test intentionally omitted for CModule, because SourceKit does not support clang USRs.

// == CursorInfo works for struct defined in SwiftModule ==
// RUN: %sourcekitd-test -req=cursor -pos=6:43 -print-raw-response -vfs-files=%t/VFS/target_file1.swift=%s,%t/VFS/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %t/VFS/target_file1.swift -- %t/VFS/target_file1.swift %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple | %FileCheck --check-prefix=CHECK-SWIFTMODULE-REF %s
// RUN: %sourcekitd-test -req=cursor -usr "s:11SwiftModule015StructDefinedInaB0V" -print-raw-response -vfs-files=%t/VFS/target_file1.swift=%s,%t/VFS/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %t/VFS/target_file1.swift -- %t/VFS/target_file1.swift %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple | %FileCheck --check-prefix=CHECK-SWIFTMODULE-DECL %s

// == CursorInfo works for struct defined in same target as primary file ==
// RUN: %sourcekitd-test -req=cursor -pos=7:43 -print-raw-response -vfs-files=%t/VFS/target_file1.swift=%s,%t/VFS/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %t/VFS/target_file1.swift -- %t/VFS/target_file1.swift %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple | %FileCheck --check-prefix=CHECK-SAMETARGET-REF %s
// RUN: %sourcekitd-test -req=cursor -usr "s:4main25StructDefinedInSameTargetV" -print-raw-response -vfs-files=%t/VFS/target_file1.swift=%s,%t/VFS/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %t/VFS/target_file1.swift -- %t/VFS/target_file1.swift %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple | %FileCheck --check-prefix=CHECK-SAMETARGET-DECL %s

// == Using an open document ==
// RUN: %sourcekitd-test \
// RUN:   -req=open -vfs-files=%t/VFS/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %s -pass-as-sourcetext -- %s %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple == \
// RUN:   -req=cursor -pos=5:43 %s -print-raw-response -- %s %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple | %FileCheck --check-prefix=CHECK-CMODULE %s

// == Using an open document without semantic info ==
// RUN: %sourcekitd-test \
// RUN:   -req=syntax-map -vfs-files=%t/VFS/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %s -pass-as-sourcetext == \
// RUN:   -req=cursor -pos=5:43 %s -print-raw-response -- %s %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple | %FileCheck --check-prefix=CHECK-CMODULE %s

// == Overriding an open document ==
// RUN: %sourcekitd-test \
// RUN:   -req=syntax-map %s -pass-as-sourcetext == \
// RUN:   -req=cursor -pos=5:43 %s -print-raw-response -vfs-files=%t/VFS/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule -- %s %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple | %FileCheck --check-prefix=CHECK-CMODULE %s
