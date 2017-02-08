// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-module-path %t/Mod.swiftmodule -module-name Mod %s
// RUN: %target-swift-frontend -typecheck -verify -I %t %S/Inputs/inheritance_protocol_multi_module_2.swift

/* module Mod */

public protocol MyProtocol  {}
open class ClassLevel1: MyProtocol {
    public init() {}
}
open class ClassLevel2: ClassLevel1 {
    public override init () {}
}
