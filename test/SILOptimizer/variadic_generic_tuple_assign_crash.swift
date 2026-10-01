// RUN: %target-swift-frontend -emit-sil %s -target %target-swift-5.9-abi-triple
// RUN: %target-swift-frontend -emit-ir %s -target %target-swift-5.9-abi-triple

// This test case reproduces a crash in DefiniteInitialization when initializing
// a struct property that is a tuple containing a pack expansion.
//
// The crash occurs when:
// 1. Struct has multiple properties including a pack expansion tuple
// 2. Pack tuple is assigned first
// 3. A throwing call comes AFTER the pack tuple assignment
//
// DI must generate cleanup code to destroy the pack tuple if the throw occurs.
// The bug was that DI incorrectly used tuple_element_addr (which requires static
// indices) instead of treating the pack tuple as a single opaque element.

func produceValue<T>(type: T.Type) -> T { fatalError() }
func throwingCall() throws -> Int { return 0 }

public struct CrashCase<each Input> {
    public let input: (repeat each Input)
    public let foo: Int

    public init() throws {
        self.input = (repeat produceValue(type: (each Input).self))
        self.foo = try throwingCall()
    }
}
