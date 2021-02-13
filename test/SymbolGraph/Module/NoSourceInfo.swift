// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Source)
// RUN: %empty-directory(%t/NoSourceInfo.framework/Modules/NoSourceInfo.swiftmodule)
// RUN: %empty-directory(%t/NoSourceInfo.framework/Modules/NoSourceInfo.swiftmodule/Project)

// RUN: cp %s %t/Source/Input.swift

// RUN: %target-swift-frontend -module-name NoSourceInfo -emit-module -emit-module-path %t/NoSourceInfo.framework/Modules/NoSourceInfo.swiftmodule/%target-swiftmodule-name -emit-module-doc-path %t/NoSourceInfo.framework/Modules/NoSourceInfo.swiftmodule/%target-swiftdoc-name -emit-module-interface-path %t/NoSourceInfo.framework/Modules/NoSourceInfo.swiftmodule/%target-swiftinterface-name -emit-module-source-info-path %t/NoSourceInfo.framework/Modules/NoSourceInfo.swiftmodule/Project/%target-swiftsourceinfo-name %t/Source/Input.swift

// RUN: mv %t/Source %t/MovedSource

// RUN: %target-swift-symbolgraph-extract -module-name NoSourceInfo -F %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/NoSourceInfo.symbols.json

// CHECK: s:12NoSourceInfo1SV
// CHECK: docComment
// CHECK: This is a test

/// This is a test
public struct S {
    /// This is also a test
    public var x: Int
}

/// writing some docs here
open class C<Data> {
    /// writing more docs over there
    public init() {}
}

open class CO: C<String> {
    override public init() { super.init() }
}
