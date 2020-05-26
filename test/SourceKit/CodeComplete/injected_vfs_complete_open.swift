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
// RUN: %sourcekitd-test -req=complete.open -pos=9:28 -vfs-files=%t/VFS/target_file1.swift=@%s,%t/VFS/target_file2.swift=@%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %t/VFS/target_file1.swift -pass-as-sourcetext -- %t/VFS/target_file1.swift %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple | %FileCheck --check-prefix=CHECK-CMODULE %s
// RUN: %sourcekitd-test -req=complete.open -pos=10:32 -vfs-files=%t/VFS/target_file1.swift=@%s,%t/VFS/target_file2.swift=@%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %t/VFS/target_file1.swift -pass-as-sourcetext -- %t/VFS/target_file1.swift %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple | %FileCheck --check-prefix=CHECK-SWIFTMODULE %s
// RUN: %sourcekitd-test -req=complete.open -pos=11:31 -vfs-files=%t/VFS/target_file1.swift=@%s,%t/VFS/target_file2.swift=@%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %t/VFS/target_file1.swift -pass-as-sourcetext -- %t/VFS/target_file1.swift %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple | %FileCheck --check-prefix=CHECK-SAMETARGET %s

// RUN: not %sourcekitd-test -req=complete.open -vfs-name nope %s -pass-as-sourcetext -dont-print-request -pos=9:27 2>&1 | %FileCheck %s -check-prefix=NONEXISTENT_VFS_ERROR
// NONEXISTENT_VFS_ERROR: error response (Request Failed): unknown virtual filesystem 'nope'

// RUN: not %sourcekitd-test -req=complete.open -pos=11:31 -vfs-files=%t/VFS/target_file1.swift=@%s,%t/VFS/target_file2.swift=@%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %t/VFS/target_file1.swift -pass-as-sourcetext -- %t/VFS/target_file1.swift %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple \
// RUN:   == -req=complete.update -pos=11:31 -vfs-files=%t/VFS/target_file1.swift=@%s,%t/VFS/target_file2.swift=@%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %t/VFS/target_file1.swift 2>&1 | %FileCheck --check-prefix=UNSUPPORTED_REQ %s
// UNSUPPORTED_REQ: error response (Request Invalid): This request does not support custom filesystems

// RUN: %sourcekitd-test -req=complete.open -pos=11:31 -dont-print-response -vfs-files=%t/VFS/target_file2.swift=@%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %s -pass-as-sourcetext -- %s %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple \
// RUN:   == -req=complete.update -pos=11:31 %s | %FileCheck --check-prefix=CHECK-SAMETARGET %s

// RUN: %sourcekitd-test -req=complete.open -pos=11:31 -dont-print-response -vfs-files=%t/VFS/target_file2.swift=@%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %s -pass-as-sourcetext -- %s %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple \
// RUN:   == -req=complete.update -pos=11:31 -req-opts=filtertext=method %s | %FileCheck --check-prefix=CHECK-SAMETARGET %s

// Inner completion.
// RUN: %sourcekitd-test -req=complete.open -pos=9:1 -req-opts=filtertext=StructDefinedInSameTarget -vfs-files=%t/VFS/target_file2.swift=@%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %s -pass-as-sourcetext -- %s %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple | %FileCheck --check-prefix=INNER_SAMETARGET %s
// RUN: %sourcekitd-test -req=complete.open -pos=9:1 -dont-print-response -vfs-files=%t/VFS/target_file2.swift=@%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %s -pass-as-sourcetext -- %s %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple \
// RUN:   == -req=complete.update -pos=9:1 -req-opts=filtertext=StructDefinedInSameTarget %s | %FileCheck --check-prefix=INNER_SAMETARGET %s
// INNER_SAMETARGET: key.name: "StructDefinedInSameTarget"
// INNER_SAMETARGET: key.name: "StructDefinedInSameTarget."

// RUN: %sourcekitd-test -req=complete.open -pos=9:1 -req-opts=filtertext=StructDefinedInCModule -vfs-files=%t/VFS/target_file2.swift=@%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %s -pass-as-sourcetext -- %s %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple | %FileCheck --check-prefix=INNER_CMODULE %s
// RUN: %sourcekitd-test -req=complete.open -pos=9:1 -dont-print-response -vfs-files=%t/VFS/target_file2.swift=@%S/../Inputs/vfs/other_file_in_target.swift,%t/VFS/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,%t/VFS/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,%t/VFS/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule %s -pass-as-sourcetext -- %s %t/VFS/target_file2.swift -I %t/VFS/CModule -I %t/VFS/SwiftModule -target %target-triple \
// RUN:   == -req=complete.update -pos=9:1 -req-opts=filtertext=StructDefinedInCModule %s | %FileCheck --check-prefix=INNER_CMODULE %s
// INNER_CMODULE: key.name: "StructDefinedInCModule"
// INNER_CMODULE: key.name: "StructDefinedInCModule."
// INNER_CMODULE: key.name: "StructDefinedInCModule("
