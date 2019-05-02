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
