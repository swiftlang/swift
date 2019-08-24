// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: foundation

// SR-5958
import Foundation

public struct Property: Equatable, Hashable, Codable {
    public var value: PropertyValue<Property>
}

public enum PropertyValue<P>: Equatable, Hashable where P: Equatable & Hashable {
    case invalid
    case date(date: Date?)
}

extension PropertyValue: Codable where P: Codable {
    public func encode(to encoder: Encoder) throws {}
    public init(from decoder: Decoder) throws { self = .invalid }
}

extension String: Error {}

let encoder = JSONEncoder()
let json = try! encoder.encode(
  Property(value: .invalid)
)

let decoder = JSONDecoder()
let result = try! decoder.decode(Property.self, from: json)
print(result)
