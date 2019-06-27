import CModule
import SwiftModule

func foo(
    _ structDefinedInCModule: StructDefinedInCModule,
    _ structDefinedInSwiftModule: StructDefinedInSwiftModule,
    _ structDefinedInSameTarget: StructDefinedInSameTarget
) {
    structDefinedInCModule.
    structDefinedInSwiftModule.
    structDefinedInSameTarget.
}

// CHECK-CMODULE: key.name: "intFieldDefinedInCModule"

// CHECK-SWIFTMODULE: key.name: "methodDefinedInSwiftModule()"

// CHECK-SAMETARGET: key.name: "methodDefinedInSameTarget()"

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/SwiftModule.swiftmodule -module-name SwiftModule %S/../Inputs/vfs/SwiftModule/SwiftModule.swift
// RUN: %sourcekitd-test -req=complete -pos=9:27 -vfs-files=/target_file1.swift=%s,/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule /target_file1.swift -pass-as-sourcetext -- /target_file1.swift /target_file2.swift -I /CModule -I /SwiftModule -target %target-triple | %FileCheck --check-prefix=CHECK-CMODULE %s
// RUN: %sourcekitd-test -req=complete -pos=10:31 -vfs-files=/target_file1.swift=%s,/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule /target_file1.swift -pass-as-sourcetext -- /target_file1.swift /target_file2.swift -I /CModule -I /SwiftModule -target %target-triple | %FileCheck --check-prefix=CHECK-SWIFTMODULE %s
// RUN: %sourcekitd-test -req=complete -pos=11:30 -vfs-files=/target_file1.swift=%s,/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule /target_file1.swift -pass-as-sourcetext -- /target_file1.swift /target_file2.swift -I /CModule -I /SwiftModule -target %target-triple | %FileCheck --check-prefix=CHECK-SAMETARGET %s

// RUN: not %sourcekitd-test -req=complete -vfs-name nope %s -pass-as-sourcetext -dont-print-request -pos=9:27 2>&1 | %FileCheck %s -check-prefix=NONEXISTENT_VFS_ERROR
// NONEXISTENT_VFS_ERROR: error response (Request Failed): unknown virtual filesystem 'nope'
