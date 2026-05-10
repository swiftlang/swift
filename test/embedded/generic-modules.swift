// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// Check that this compiles successfully.

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -c -o %t/Main.o -I %t %t/Main.swift -enable-experimental-feature Embedded -parse-as-library

// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

public func foo<Info: Collection>(info: Info) {
    let b = MyContainer()
    _ = b.dropFirst()
}

struct MyContainer {
    var x = 42
    init() { }
}

extension MyContainer: Collection {
    typealias Index = Int
    var startIndex: Index { fatalError() }
    var endIndex: Index { fatalError() }
    
    subscript(_ index: Index) -> UInt8 {
        get {
            fatalError()
        }
        set {
            fatalError()
        }
    }
    
    func index(after i: Int) -> Int {
        fatalError()
    }
}

// BEGIN Main.swift

import MyModule

public func main() {
  foo(info: [1, 2, 3])
}

