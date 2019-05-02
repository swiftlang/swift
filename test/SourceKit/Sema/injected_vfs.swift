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
// RUN: %swift -emit-module -o %t/SwiftModule.swiftmodule -module-name SwiftModule %S/../Inputs/vfs/SwiftModule/SwiftModule.swift
// RUN: %sourcekitd-test -req=open -vfs-files=/target_file1.swift=%s,/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule /target_file1.swift -- /target_file1.swift /target_file2.swift -I /CModule -I /SwiftModule == \
// RUN:    -req=print-diags -vfs-files=/target_file1.swift=%s,/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift,/CModule/module.modulemap=%S/../Inputs/vfs/CModule/module.modulemap,/CModule/CModule.h=%S/../Inputs/vfs/CModule/CModule.h,/SwiftModule/SwiftModule.swiftmodule=%t/SwiftModule.swiftmodule /target_file1.swift | %FileCheck %s
