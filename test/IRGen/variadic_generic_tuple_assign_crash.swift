// RUN: %target-swift-frontend -emit-ir %s -target %target-swift-5.9-abi-triple

// This test case reproduces a crash in projectTupleElementAddress during IRGen
// when assigning a pack expansion to a tuple property via an unkeyed container.

import Foundation

public struct CorpusEntry<each Input: Codable & Sendable>: Sendable, Codable {
    public let input: (repeat each Input)
    public let foo: Int

    public func encode(to encoder: any Encoder) throws {
        var container = encoder.container(keyedBy: CorpusEntryCodingKeys.self)
        var dataList = [Data]()
        (repeat try dataList.append(JSONEncoder().encode(each input)))

        try container.encode(dataList, forKey: .input)
        try container.encode(foo, forKey: .foo)
    }

    public init(from decoder: any Decoder) throws {
        let container = try decoder.container(keyedBy: CorpusEntryCodingKeys.self)
        var contentsContainer = try container.nestedUnkeyedContainer(forKey: .input)

        self.input = try (repeat contentsContainer.decode((each Input).self))

        self.foo = try container.decode(Int.self, forKey: .foo)
    }
}

public enum CorpusEntryCodingKeys: String, CodingKey {
    case input
    case foo
}
