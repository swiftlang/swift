// RUN: %target-typecheck-verify-swift

struct StructWithNonExcludedLetMembers : Codable { // expected-error {{type 'StructWithNonExcludedLetMembers' does not conform to protocol 'Decodable'}}
    // expected-error@-1 {{type 'StructWithNonExcludedLetMembers' does not conform to protocol 'Encodable'}}

    // Suppress the memberwise initializer. Optional `let`s do not get an implicit nil assignment.
    init() {}

    let p1: String?
    let p2: String!

    // AnyHashable does not conform to Codable.
    let p3: AnyHashable? // expected-note {{cannot automatically synthesize 'Decodable' because 'AnyHashable?' does not conform to 'Decodable'}}
    // expected-note@-1 {{cannot automatically synthesize 'Encodable' because 'AnyHashable?' does not conform to 'Encodable'}}
    let p4: AnyHashable! // expected-note {{cannot automatically synthesize 'Decodable' because 'AnyHashable!' does not conform to 'Decodable'}}
    // expected-note@-1 {{cannot automatically synthesize 'Encodable' because 'AnyHashable!' does not conform to 'Encodable'}}
}

struct StructWithExcludedLetMembers : Codable { // expected-error {{type 'StructWithExcludedLetMembers' does not conform to protocol 'Decodable'}}

    // Suppress the memberwise initializer. Optional `let`s do not get an implicit nil assignment.
    init() {}

    let p1: String?
    let p2: String!

    // AnyHashable does not conform to Codable.
    let p3: AnyHashable? // expected-note {{cannot automatically synthesize 'Decodable' because 'p3' does not have a matching CodingKey and does not have a default value}}
    let p4: AnyHashable! // expected-note {{cannot automatically synthesize 'Decodable' because 'p4' does not have a matching CodingKey and does not have a default value}}

    // Explicitly exclude non-Codable properties.
    enum CodingKeys : String, CodingKey {
        case p1, p2
    }
}

struct StructWithNonExcludedVarMembers : Codable { // expected-error {{type 'StructWithNonExcludedVarMembers' does not conform to protocol 'Decodable'}}
    // expected-error@-1 {{type 'StructWithNonExcludedVarMembers' does not conform to protocol 'Encodable'}}

    // Suppress the memberwise initializer.
    init() {}

    var p1: String?
    var p2: String!

    // AnyHashable does not conform to Codable.
    var p3: AnyHashable? // expected-note {{cannot automatically synthesize 'Decodable' because 'AnyHashable?' does not conform to 'Decodable'}}
    // expected-note@-1 {{cannot automatically synthesize 'Encodable' because 'AnyHashable?' does not conform to 'Encodable'}}
    var p4: AnyHashable! // expected-note {{cannot automatically synthesize 'Decodable' because 'AnyHashable!' does not conform to 'Decodable'}}
    // expected-note@-1 {{cannot automatically synthesize 'Encodable' because 'AnyHashable!' does not conform to 'Encodable'}}
}

struct StructWithExcludedVarMembers : Codable {

    // Suppress the memberwise initializer.
    init() {}

    var p1: String?
    var p2: String!

    // AnyHashable does not conform to Codable.
    var p3: AnyHashable?
    var p4: AnyHashable!

    // Explicitly exclude non-Codable properties.
    enum CodingKeys : String, CodingKey {
        case p1, p2
    }
}
