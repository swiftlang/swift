// RUN: %target-run-simple-swift(-target %target-swift-5.9-abi-triple) | %FileCheck %s

// https://github.com/swiftlang/swift/issues/78598

// REQUIRES: executable_test

// This test needs a Swift 5.9 runtime or newer.
// UNSUPPORTED: back_deployment_runtime

var counter = 0

final class Entry<Results> {
    var isEmpty: Bool { true }
    init() {
        counter += 1
    }
    deinit {
        counter -= 1
    }
}

struct Foo<each Results> {
    private let entry: (repeat Entry<each Results>)
    
    init(_ entry: (repeat Entry<each Results>)) {
        self.entry = entry
        for entry in repeat each entry {
            if entry.isEmpty {
                break
            }
        }
    }
    
    var hmmm: Void {
        for entry in repeat each entry {
            if !entry.isEmpty {
                break
            }
        }
    }
}

_ = Foo((
    Entry<Any>()
)).hmmm

// CHECK: Counter: 0
print("Counter: \(counter)")
