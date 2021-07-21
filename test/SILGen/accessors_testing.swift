// RUN: %target-swift-emit-silgen -parse-as-library -module-name accessors %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -parse-as-library -enable-testing -module-name accessors %s | %FileCheck %s

// rdar://78523318: Ensure that private(set) accessors for internal or more
// visible properties have hidden linkage, because other code inside the module
// needs to reference the setter to form a key path.

public struct Foo {
    // CHECK-LABEL: sil hidden [ossa] @$s9accessors3FooV6internSivs
    private(set) internal var intern: Int {
        get { return 0 }
        set {}
    }
    // CHECK-LABEL: sil hidden [ossa] @$s9accessors3FooV3pubSivs
    private(set) public var pub: Int {
        get { return 0 }
        set {}
    }

    public mutating func exercise() {
        _ = intern
        _ = pub
        intern = 0
        pub = 0
    }
}

