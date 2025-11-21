// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -parse-as-library -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -O
// RUN: %target-swift-frontend -parse-as-library -I %t %t/Main.swift -O -emit-ir | %FileCheck %s

// BEGIN MyModule.swift

public struct NonResilientTypeContainingAResilientType {
    var m: Mirror? = nil
}

// BEGIN Main.swift

import MyModule

public protocol MyProtocol {}

public struct MyStruct: MyProtocol {
    public var x: NonResilientTypeContainingAResilientType? = nil
}

public let glob: [any MyProtocol.Type] = [MyStruct.self]

// CHECK: @"$s4Main4globSayAA10MyProtocol_pXpGvp" = {{.*}}global %TSa zeroinitializer
