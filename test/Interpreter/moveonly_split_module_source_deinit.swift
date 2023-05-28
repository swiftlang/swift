// Normal test.

// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -emit-module -module-name server -emit-module-path %t/server.swiftmodule %s %S/Inputs/moveonly_split_module_source_input.swift
// RUN: %target-swiftc_driver  -emit-executable -module-name server -emit-module-path %t/server.swiftmodule %s %S/Inputs/moveonly_split_module_source_input.swift -o %t/server
// RUN: %target-codesign %t/server
// RUN: %target-run %t/server | %FileCheck %s

// REQUIRES: executable_test

@main
public struct server {
    public static func main() throws {
        let server = MoveOnly() // CHECK: ==> I am in the deinit!
    }
}
