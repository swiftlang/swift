// RUN: %target-swift-frontend -O -emit-ir %S/Inputs/foundation_bridging.swift -primary-file %s -disable-autolink-framework CoreFoundation -parse-as-library -module-name Foundation | %FileCheck %s

// REQUIRES: OS=windows-msvc

open class NSSet {
    public init(_ set: AnyHashable) {
        if let item = set as? NSObject {
            print("yay, has object!")    
        }
    }
}

// Ensure that the _bridgeToObjectiveC function (from the same module) is not annotated
// with 'dllimport'.
// CHECK: declare swiftcc ptr @"$ss11AnyHashableV10FoundationE19_bridgeToObjectiveCAC8NSObjectCyF"
