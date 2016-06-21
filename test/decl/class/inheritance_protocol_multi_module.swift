// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-module-path %t/Mod.swiftmodule -module-name Mod %s
// RUN: %target-swift-frontend -parse -verify -I %t %S/Inputs/inheritance_protocol_multi_module_2.swift

/* module Mod */

public protocol MyProtocol  {}
public class ClassLevel1: MyProtocol {
    public init() {}
}
public class ClassLevel2: ClassLevel1 {
    public override init () {}
}
