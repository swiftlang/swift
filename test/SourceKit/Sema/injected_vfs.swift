import CModule
import SwiftModule

func foo(
    _ structDefinedInSwiftModule: StructDefinedInSwiftModule,
    _ structDefinedInSameTarget: StructDefinedInSameTarget
) {
    let a: String = functionDefinedInCModule()
    // CHECK: cannot convert value of type 'Void' to specified type 'String'

    let b: Float = structDefinedInSwiftModule.methodDefinedInSwiftModule()
    // CHECK: cannot convert value of type '()' to specified type 'Float'

    let c: Double = structDefinedInSameTarget.methodDefinedInSameTarget()
    // CHECK: cannot convert value of type '()' to specified type 'Double'
}

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/SwiftModule.swiftmodule -module-name SwiftModule %S/../Inputs/vfs/SwiftModule/SwiftModule.swift
// RUN: %sourcekitd-test -req=open -vfs-files=%t/VFS/target_file1.swift=%s,%t/VFS/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %t/VFS/target_file1.swift -pass-as-sourcetext -- %t/VFS/target_file1.swift %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple == \
// RUN:    -req=print-diags -vfs-files=%t/VFS/target_file1.swift=%s,%t/VFS/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %t/VFS/target_file1.swift  | %FileCheck %s

// RUN: not %sourcekitd-test -req=syntax-map -vfs-files=%t/VFS/target_file1.swift=%s %t/VFS/target_file1.swift -dont-print-request 2>&1 | %FileCheck %s -check-prefix=SOURCEFILE_ERROR
// SOURCEFILE_ERROR: error response (Request Failed): using 'key.sourcefile' to read source text from the filesystem

// RUN: not %sourcekitd-test -req=syntax-map  -vfs-name nope %s -pass-as-sourcetext -dont-print-request 2>&1 | %FileCheck %s -check-prefix=NONEXISTENT_VFS_ERROR
// NONEXISTENT_VFS_ERROR: error response (Request Failed): unknown virtual filesystem 'nope'

// == Close the document and reopen with a new VFS (modules) ==
// RUN: %sourcekitd-test -req=open -vfs-files=%t/VFS/target_file1.swift=%s,%t/VFS/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %t/VFS/target_file1.swift -pass-as-sourcetext -- %t/VFS/target_file1.swift %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple == \
// RUN:    -req=close -name %t/VFS/target_file1.swift == \
// RUN:    -req=open -vfs-files=%t/VFS/target_file1.swift=%s,%t/VFS/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift %t/VFS/target_file1.swift -pass-as-sourcetext -- %t/VFS/target_file1.swift %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple == \
// RUN:    -req=print-diags -vfs-files=%t/VFS/target_file1.swift=%s %t/VFS/target_file1.swift %t/VFS/target_file1.swift  | %FileCheck %s -check-prefix=NO_MODULES_VFS
// NO_MODULES_VFS: no such module 'CModule'

// == Close the document and reopen with a new VFS (inputs) ==
// RUN: %sourcekitd-test -req=open -vfs-files=%t/VFS/target_file1.swift=%s,%t/VFS/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %t/VFS/target_file1.swift -pass-as-sourcetext -- %t/VFS/target_file1.swift %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple == \
// RUN:    -req=close -name %t/VFS/target_file1.swift == \
// RUN:    -req=open -vfs-files=%t/VFS/target_file1.swift=%s,%t/VFS/target_file2.swift=%S/../Inputs/vfs/other_file_in_target_2.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %t/VFS/target_file1.swift -pass-as-sourcetext -- %t/VFS/target_file1.swift %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple == \
// RUN:    -req=print-diags -vfs-files=%t/VFS/target_file1.swift=%s %t/VFS/target_file1.swift %t/VFS/target_file1.swift  | %FileCheck %s -check-prefix=TARGET_FILE_2_MOD
// TARGET_FILE_2_MOD: cannot convert value of type 'Void' to specified type 'String'
// TARGET_FILE_2_MOD: cannot convert value of type '()' to specified type 'Float'
// TARGET_FILE_2_MOD: cannot convert value of type 'Int' to specified type 'Double'

// == Reopen with a new VFS without closing ==
// RUN: %sourcekitd-test -req=open -vfs-files=%t/VFS/target_file1.swift=%s,%t/VFS/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %t/VFS/target_file1.swift -pass-as-sourcetext -- %t/VFS/target_file1.swift %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple == \
// RUN:    -req=open -vfs-files=%t/VFS/target_file1.swift=%s,%t/VFS/target_file2.swift=%S/../Inputs/vfs/other_file_in_target_2.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %t/VFS/target_file1.swift -pass-as-sourcetext -- %t/VFS/target_file1.swift %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple == \
// RUN:    -req=print-diags -vfs-files=%t/VFS/target_file1.swift=%s %t/VFS/target_file1.swift %t/VFS/target_file1.swift  | %FileCheck %s -check-prefix=TARGET_FILE_2_MOD
