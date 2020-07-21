// RUN: %target-typecheck-verify-swift

class ClassWithNonExcludedLetMembers : Codable { // expected-error {{class 'ClassWithNonExcludedLetMembers' has no initializers}}
    // expected-error@-1 {{type 'ClassWithNonExcludedLetMembers' does not conform to protocol 'Decodable'}}
    // expected-error@-2 {{type 'ClassWithNonExcludedLetMembers' does not conform to protocol 'Encodable'}}

    // Optional `let`s do not get an implicit nil assignment.
    let p1: String? // expected-note {{stored property 'p1' without initial value prevents synthesized initializers}}
    let p2: String! // expected-note {{stored property 'p2' without initial value prevents synthesized initializers}}

    // AnyHashable does not conform to Codable.
    let p3: AnyHashable? // expected-note {{stored property 'p3' without initial value prevents synthesized initializers}}
    // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'AnyHashable?' does not conform to 'Decodable'}}
    // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'AnyHashable?' does not conform to 'Encodable'}}
    let p4: AnyHashable? // expected-note {{stored property 'p4' without initial value prevents synthesized initializers}}
    // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'AnyHashable?' does not conform to 'Decodable'}}
    // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'AnyHashable?' does not conform to 'Encodable'}}
}

class ClassWithExcludedLetMembers : Codable { // expected-error {{class 'ClassWithExcludedLetMembers' has no initializers}}
    // expected-error@-1 {{type 'ClassWithExcludedLetMembers' does not conform to protocol 'Decodable'}}

    // Optional `let`s do not get an implicit nil assignment.
    let p1: String? // expected-note {{stored property 'p1' without initial value prevents synthesized initializers}}
    let p2: String! // expected-note {{stored property 'p2' without initial value prevents synthesized initializers}}

    // AnyHashable does not conform to Codable.
    let p3: AnyHashable? // expected-note {{stored property 'p3' without initial value prevents synthesized initializers}}
    // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'p3' does not have a matching CodingKey and does not have a default value}}
    let p4: AnyHashable? // expected-note {{stored property 'p4' without initial value prevents synthesized initializers}}
    // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'p4' does not have a matching CodingKey and does not have a default value}}

    // Explicitly exclude non-Codable properties.
    enum CodingKeys : String, CodingKey {
        case p1, p2
    }
}

class ClassWithNonExcludedVarMembers : Codable { // expected-error {{type 'ClassWithNonExcludedVarMembers' does not conform to protocol 'Decodable'}}
    // expected-error@-1 {{type 'ClassWithNonExcludedVarMembers' does not conform to protocol 'Encodable'}}

    var p1: String?
    var p2: String!

    // AnyHashable does not conform to Codable.
    var p3: AnyHashable? // expected-note {{cannot automatically synthesize 'Decodable' because 'AnyHashable?' does not conform to 'Decodable'}}
    // expected-note@-1 {{cannot automatically synthesize 'Encodable' because 'AnyHashable?' does not conform to 'Encodable'}}
    var p4: AnyHashable! // expected-note {{cannot automatically synthesize 'Decodable' because 'AnyHashable!' does not conform to 'Decodable'}}
    // expected-note@-1 {{cannot automatically synthesize 'Encodable' because 'AnyHashable!' does not conform to 'Encodable'}}
}

class ClassWithExcludedVarMembers : Codable {
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
