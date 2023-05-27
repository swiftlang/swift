// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -emit-module -module-name server -emit-module-path %t/server.swiftmodule %s %S/Inputs/moveonly_split_module_source_input.swift
// RUN: %target-swiftc_driver  -emit-executable -module-name server -emit-module-path %t/server.swiftmodule %s %S/Inputs/moveonly_split_module_source_input.swift -Xlinker -add_ast_path -Xlinker %t/server.swiftmodule -Xlinker -alias -Xlinker _server_main -Xlinker _main -emit-ir | %FileCheck %s

// Make sure we call the deinit through the value witness table.

// CHECK-LABEL: define swiftcc void @"$s6serverAAV4mainyyKFZ"(%swift.refcounted* swiftself %0, %swift.error** noalias nocapture swifterror dereferenceable(8) %1) #0 {
@main
public struct server {
    public static func main() throws {
        let server = MoveOnly()
    }
}
