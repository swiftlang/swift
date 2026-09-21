// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library -O
// RUN: %target-swift-frontend -c -I %t %t/Main.swift -enable-experimental-feature Embedded -O -o /dev/null

// REQUIRES: OS=macosx || OS=linux-gnu || OS=wasip1
// REQUIRES: swift_feature_Embedded

//--- MyModule.swift

public struct ParseContainerStack {
    var stack: [Int] = [0]
    public init() {}
    public mutating func run() {
        let i = stack.first
        if let i {
            _ = i
            for _ in 0..<stack.count { }
        }
    }
}

//--- Main.swift

import MyModule

public func test() {
    var s = ParseContainerStack()
    s.run()
}
