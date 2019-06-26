import CModule
import SwiftModule

func foo(
    _ structDefinedInCModule: StructDefinedInCModule,
    _ structDefinedInSwiftModule: StructDefinedInSwiftModule,
    _ structDefinedInSameTarget: StructDefinedInSameTarget
) {
}

// CHECK-CMODULE: <Declaration>struct StructDefinedInCModule</Declaration>

// CHECK-SWIFTMODULE: <Declaration>struct StructDefinedInSwiftModule</Declaration>

// CHECK-SAMETARGET: <Declaration>struct StructDefinedInSameTarget</Declaration>

// RUN: %empty-directory(%t)
// RUN: %swift -emit-module -o %t/SwiftModule.swiftmodule -module-name SwiftModule %S/../Inputs/vfs/SwiftModule/SwiftModule.swift
// RUN: %sourcekitd-test -req=cursor -pos=5:43 -vfs-files=/target_file1.swift=%s,/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule /target_file1.swift -- /target_file1.swift /target_file2.swift -I /CModule -I /SwiftModule | %FileCheck --check-prefix=CHECK-CMODULE %s
// USR test intentionally omitted for CModule, because SourceKit does not support clang USRs.
// RUN: %sourcekitd-test -req=cursor -pos=6:43 -vfs-files=/target_file1.swift=%s,/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule /target_file1.swift -- /target_file1.swift /target_file2.swift -I /CModule -I /SwiftModule | %FileCheck --check-prefix=CHECK-SWIFTMODULE %s
// RUN: %sourcekitd-test -req=cursor -usr "s:11SwiftModule015StructDefinedInaB0V" -vfs-files=/target_file1.swift=%s,/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule /target_file1.swift -- /target_file1.swift /target_file2.swift -I /CModule -I /SwiftModule | %FileCheck --check-prefix=CHECK-SWIFTMODULE %s
// RUN: %sourcekitd-test -req=cursor -pos=7:43 -vfs-files=/target_file1.swift=%s,/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule /target_file1.swift -- /target_file1.swift /target_file2.swift -I /CModule -I /SwiftModule | %FileCheck --check-prefix=CHECK-SAMETARGET %s
// RUN: %sourcekitd-test -req=cursor -usr "s:4main25StructDefinedInSameTargetV" -vfs-files=/target_file1.swift=%s,/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule /target_file1.swift -- /target_file1.swift /target_file2.swift -I /CModule -I /SwiftModule | %FileCheck --check-prefix=CHECK-SAMETARGET %s

// RUN: not %sourcekitd-test -req=cursor -vfs-name nope %s -dont-print-request 2>&1 | %FileCheck %s -check-prefix=NONEXISTENT_VFS_ERROR
// NONEXISTENT_VFS_ERROR: error response (Request Failed): unknown virtual filesystem 'nope'

// == Using an open document ==
// RUN: %sourcekitd-test \
// RUN:   -req=open -vfs-files=/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %s -pass-as-sourcetext -- %s /target_file2.swift -I /CModule -I /SwiftModule == \
// RUN:   -req=cursor -pos=5:43 %s -print-raw-response -- %s /target_file2.swift -I /CModule -I /SwiftModule | %FileCheck --check-prefix=CMODULE_RAW %s
// CMODULE_RAW: key.kind: source.lang.swift.ref.struct
// CMODULE_RAW: key.name: "StructDefinedInCModule"
// CMODULE_RAW: key.filepath: "/CModule/CModule.h"

// == Overriding an open document ==
// RUN: %sourcekitd-test \
// RUN:   -req=syntax-map %s -pass-as-sourcetext == \
// RUN:   -req=cursor -pos=5:43 %s -print-raw-response -vfs-files=/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule -- %s /target_file2.swift -I /CModule -I /SwiftModule | %FileCheck --check-prefix=CMODULE_RAW %s
