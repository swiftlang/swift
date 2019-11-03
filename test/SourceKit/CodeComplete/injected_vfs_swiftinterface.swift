import SwiftModule

func foo(
    _ structDefinedInSwiftModule: StructDefinedInSwiftModule
) {
    structDefinedInSwiftModule.
}

// CHECK: key.name: "methodDefinedInSwiftModule()"

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-interface-path %t/SwiftModule.swiftinterface -module-name SwiftModule -emit-module -o /dev/null %S/../Inputs/vfs/SwiftModule/SwiftModule.swift
// RUN: %sourcekitd-test -req=complete -pos=6:31 -vfs-files=/target_file1.swift=%s,/SwiftModule/SwiftModule.swiftinterface=%t/SwiftModule.swiftinterface /target_file1.swift -pass-as-sourcetext -- /target_file1.swift -I /SwiftModule -target %target-triple | %FileCheck %s
