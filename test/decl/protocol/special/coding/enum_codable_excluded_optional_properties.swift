// RUN: %target-typecheck-verify-swift

enum EnumWithNonExcludedOptionalParameters : Codable { // expected-error {{type 'EnumWithNonExcludedOptionalParameters' does not conform to protocol 'Decodable'}}
    // expected-error@-1 {{type 'EnumWithNonExcludedOptionalParameters' does not conform to protocol 'Encodable'}}

    case x(
        p1: String?,
        p2: String!,
        // AnyHashable does not conform to Codable.
        p3: AnyHashable?, // expected-note {{cannot automatically synthesize 'Decodable' because 'AnyHashable?' does not conform to 'Decodable'}}
        // expected-note@-1 {{cannot automatically synthesize 'Encodable' because 'AnyHashable?' does not conform to 'Encodable'}}
        p4: AnyHashable!) // expected-note {{cannot automatically synthesize 'Decodable' because 'AnyHashable!' does not conform to 'Decodable'}}
        // expected-note@-1 {{cannot automatically synthesize 'Encodable' because 'AnyHashable!' does not conform to 'Encodable'}}
}

enum EnumWithExcludedOptionalParameters : Codable { // expected-error {{type 'EnumWithExcludedOptionalParameters' does not conform to protocol 'Decodable'}}
    case x(
        p1: String?,
        p2: String!,
        // AnyHashable does not conform to Codable.
        p3: AnyHashable?, // expected-note {{cannot automatically synthesize 'Decodable' because 'p3' does not have a matching CodingKey and does not have a default value}}
        p4: AnyHashable!) // expected-note {{cannot automatically synthesize 'Decodable' because 'p4' does not have a matching CodingKey and does not have a default value}}

    // Explicitly exclude non-Codable properties.
    enum XCodingKeys : String, CodingKey {
        case p1, p2
    }
}
