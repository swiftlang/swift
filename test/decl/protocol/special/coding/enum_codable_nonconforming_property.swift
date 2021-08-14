// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

enum NonCodable : Hashable {
    func hash(into hasher: inout Hasher) {}

    static func ==(_ lhs: NonCodable, _ rhs: NonCodable) -> Bool {
        return true
    }
}

struct CodableGeneric<T> : Codable {
}

// Enums whose properties are not all Codable should fail to synthesize
// conformance.
enum NonConformingEnum : Codable { // expected-error {{type 'NonConformingEnum' does not conform to protocol 'Decodable'}}
  // expected-error@-1 {{type 'NonConformingEnum' does not conform to protocol 'Decodable'}}
  // expected-error@-2 {{type 'NonConformingEnum' does not conform to protocol 'Encodable'}}
  // expected-error@-3 {{type 'NonConformingEnum' does not conform to protocol 'Encodable'}}
  case x(
      w: NonCodable, // expected-note {{cannot automatically synthesize 'Decodable' because 'NonCodable' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'NonCodable' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'NonCodable' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because 'NonCodable' does not conform to 'Encodable'}}
      x: Int,
      y: Double,

      // FIXME: Remove when conditional conformance lands.
      // Because conditional conformance is not yet available, Optional, Array,
      // Set, and Dictionary all conform to Codable even when their generic
      // parameters do not.
      // We want to make sure that these cases prevent derived conformance.
      nonCodableOptional: NonCodable? = nil, // expected-note {{cannot automatically synthesize 'Decodable' because 'NonCodable?' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'NonCodable?' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'NonCodable?' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because 'NonCodable?' does not conform to 'Encodable'}}
      nonCodableArray: [NonCodable] = [], // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable]' does not conform to 'Encodable'}}
      nonCodableSet: Set<NonCodable> = [], // expected-note {{cannot automatically synthesize 'Decodable' because 'Set<NonCodable>' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'Set<NonCodable>' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'Set<NonCodable>' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because 'Set<NonCodable>' does not conform to 'Encodable'}}
      nonCodableDictionary1: [String : NonCodable] = [:], // expected-note {{cannot automatically synthesize 'Decodable' because '[String : NonCodable]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[String : NonCodable]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[String : NonCodable]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[String : NonCodable]' does not conform to 'Encodable'}}
      nonCodableDictionary2: [NonCodable : String] = [:], // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : String]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : String]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : String]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : String]' does not conform to 'Encodable'}}
      nonCodableDictionary3: [NonCodable : NonCodable] = [:], // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : NonCodable]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : NonCodable]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : NonCodable]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : NonCodable]' does not conform to 'Encodable'}}

      // These conditions should apply recursively, too.
      nonCodableOptionalOptional: NonCodable?? = nil, // expected-note {{cannot automatically synthesize 'Decodable' because 'NonCodable??' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'NonCodable??' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'NonCodable??' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because 'NonCodable??' does not conform to 'Encodable'}}
      nonCodableOptionalArray: [NonCodable]? = nil, // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable]?' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable]?' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable]?' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable]?' does not conform to 'Encodable'}}
      nonCodableOptionalSet: Set<NonCodable>? = nil, // expected-note {{cannot automatically synthesize 'Decodable' because 'Set<NonCodable>?' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'Set<NonCodable>?' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'Set<NonCodable>?' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because 'Set<NonCodable>?' does not conform to 'Encodable'}}
      nonCodableOptionalDictionary1: [String : NonCodable]? = nil, // expected-note {{cannot automatically synthesize 'Decodable' because '[String : NonCodable]?' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[String : NonCodable]?' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[String : NonCodable]?' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[String : NonCodable]?' does not conform to 'Encodable'}}
      nonCodableOptionalDictionary2: [NonCodable : String]? = nil, // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : String]?' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : String]?' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : String]?' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : String]?' does not conform to 'Encodable'}}
      nonCodableOptionalDictionary3: [NonCodable : NonCodable]? = nil, // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : NonCodable]?' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : NonCodable]?' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : NonCodable]?' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : NonCodable]?' does not conform to 'Encodable'}}

      nonCodableArrayOptional: [NonCodable?] = [], // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable?]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable?]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable?]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable?]' does not conform to 'Encodable'}}
      nonCodableArrayArray: [[NonCodable]] = [], // expected-note {{cannot automatically synthesize 'Decodable' because '[[NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[[NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[[NonCodable]]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[[NonCodable]]' does not conform to 'Encodable'}}
      nonCodableArraySet: [Set<NonCodable>] = [], // expected-note {{cannot automatically synthesize 'Decodable' because '[Set<NonCodable>]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[Set<NonCodable>]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[Set<NonCodable>]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[Set<NonCodable>]' does not conform to 'Encodable'}}
      nonCodableArrayDictionary1: [[String : NonCodable]] = [], // expected-note {{cannot automatically synthesize 'Decodable' because '[[String : NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[[String : NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[[String : NonCodable]]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[[String : NonCodable]]' does not conform to 'Encodable'}}
      nonCodableArrayDictionary2: [[NonCodable : String]] = [], // expected-note {{cannot automatically synthesize 'Decodable' because '[[NonCodable : String]]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[[NonCodable : String]]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[[NonCodable : String]]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[[NonCodable : String]]' does not conform to 'Encodable'}}
      nonCodableArrayDictionary3: [[NonCodable : NonCodable]] = [], // expected-note {{cannot automatically synthesize 'Decodable' because '[[NonCodable : NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[[NonCodable : NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[[NonCodable : NonCodable]]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[[NonCodable : NonCodable]]' does not conform to 'Encodable'}}

      nonCodableDictionaryOptional1: [String : NonCodable?] = [:], // expected-note {{cannot automatically synthesize 'Decodable' because '[String : NonCodable?]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[String : NonCodable?]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[String : NonCodable?]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[String : NonCodable?]' does not conform to 'Encodable'}}
      nonCodableDictionaryOptional2: [NonCodable : NonCodable?] = [:], // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : NonCodable?]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : NonCodable?]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : NonCodable?]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : NonCodable?]' does not conform to 'Encodable'}}
      nonCodableDictionaryArray1: [String : [NonCodable]] = [:], // expected-note {{cannot automatically synthesize 'Decodable' because '[String : [NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[String : [NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[String : [NonCodable]]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[String : [NonCodable]]' does not conform to 'Encodable'}}
      nonCodableDictionaryArray2: [NonCodable : [NonCodable]] = [:], // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : [NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : [NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : [NonCodable]]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : [NonCodable]]' does not conform to 'Encodable'}}
      nonCodableDictionarySet1: [String : Set<NonCodable>] = [:], // expected-note {{cannot automatically synthesize 'Decodable' because '[String : Set<NonCodable>]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[String : Set<NonCodable>]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[String : Set<NonCodable>]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[String : Set<NonCodable>]' does not conform to 'Encodable'}}
      nonCodableDictionarySet2: [NonCodable : Set<NonCodable>] = [:], // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : Set<NonCodable>]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : Set<NonCodable>]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : Set<NonCodable>]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : Set<NonCodable>]' does not conform to 'Encodable'}}
      nonCodableDictionaryDictionary1: [String : [String : NonCodable]] = [:], // expected-note {{cannot automatically synthesize 'Decodable' because '[String : [String : NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[String : [String : NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[String : [String : NonCodable]]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[String : [String : NonCodable]]' does not conform to 'Encodable'}}
      nonCodableDictionaryDictionary2: [NonCodable : [String : NonCodable]] = [:], // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : [String : NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : [String : NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : [String : NonCodable]]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : [String : NonCodable]]' does not conform to 'Encodable'}}
      nonCodableDictionaryDictionary3: [String : [NonCodable : NonCodable]] = [:], // expected-note {{cannot automatically synthesize 'Decodable' because '[String : [NonCodable : NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[String : [NonCodable : NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[String : [NonCodable : NonCodable]]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[String : [NonCodable : NonCodable]]' does not conform to 'Encodable'}}
      nonCodableDictionaryDictionary4: [NonCodable : [NonCodable : NonCodable]] = [:], // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : [NonCodable : NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : [NonCodable : NonCodable]]' does not conform to 'Decodable'}}
      // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : [NonCodable : NonCodable]]' does not conform to 'Encodable'}}
      // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : [NonCodable : NonCodable]]' does not conform to 'Encodable'}}

      // However, arbitrary generic types which _do_ conform to Codable should be
      // valid.
      codableGenericThing1: CodableGeneric<NonCodable>? = nil,
      codableGenericThing2: CodableGeneric<NonCodable?>? = nil,
      codableGenericThing3: CodableGeneric<[NonCodable]>? = nil,
      codableGenericThing4: CodableGeneric<Set<NonCodable>>? = nil,
      codableGenericThing5: CodableGeneric<[String : NonCodable]>? = nil,
      codableGenericThing6: CodableGeneric<[NonCodable : String]>? = nil,
      codableGenericThing7: CodableGeneric<[NonCodable : NonCodable]>? = nil)
}

// They should not receive Codable methods.
let _ = NonConformingEnum.init(from:) // expected-error {{'NonConformingEnum' cannot be constructed because it has no accessible initializers}}
let _ = NonConformingEnum.encode(to:) // expected-error {{type 'NonConformingEnum' has no member 'encode(to:)'}}

// They should not get a CodingKeys type.
let _ = NonConformingEnum.XCodingKeys.self // expected-error {{'XCodingKeys' is inaccessible due to 'private' protection level}}
