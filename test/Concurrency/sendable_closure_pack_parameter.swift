// RUN: %target-swift-frontend -emit-sil -swift-version 6 %s -o /dev/null -verify

// REQUIRES: concurrency

// This test verifies that we produce a proper diagnostic when a non-@Sendable
// closure is used where a @Sendable closure is expected, in the context of
// parameter packs. Previously this would fail with "failed to produce diagnostic
// for expression" because the constraint system would generate multiple solutions
// with the same fix kind but at different locators.

public protocol Fuzzable {
    static var fuzz: [Self] { get }
}

public protocol Mutator<Value>: Sendable {
    associatedtype Value: Sendable
    var seeds: [Value] { get }
    func mutate(_ value: Value) -> [Value]
}

public actor FuzzEngine<each Input: Fuzzable & Codable & Sendable> {
    public typealias MutatorMutate = @Sendable ((repeat each Input)) -> [(repeat each Input)]

    private let mutatorMutate: MutatorMutate?

    public init<each M: Mutator>(
        mutators: (repeat each M)?,
        config: Int = 0
    ) where (repeat (each M).Value) == (repeat each Input) {
        self.mutatorMutate = mutators.map { mutators in
            // expected-error@-1 {{converting non-Sendable function value to '@Sendable ((repeat each Input)) -> [(repeat each Input)]' may introduce data races}}
            { input in
                let _ = Self.identity(mutators: mutators)
                return []
            }
        }
    }

    private static func identity<each M: Mutator>(mutators: (repeat each M)) -> Int { 0 }
}
