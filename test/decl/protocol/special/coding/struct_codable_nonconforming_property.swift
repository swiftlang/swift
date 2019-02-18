// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

struct NonCodable : Hashable {
    func hash(into hasher: inout Hasher) {}

    static func ==(_ lhs: NonCodable, _ rhs: NonCodable) -> Bool {
        return true
    }
}

struct CodableGeneric<T> : Codable {
    let value: Int = 5
}

// Structs whose properties are not all Codable should fail to synthesize
// conformance.
struct NonConformingStruct : Codable { // expected-error {{type 'NonConformingStruct' does not conform to protocol 'Decodable'}}
  // expected-error@-1 {{type 'NonConformingStruct' does not conform to protocol 'Decodable'}}
  // expected-error@-2 {{type 'NonConformingStruct' does not conform to protocol 'Encodable'}}
  // expected-error@-3 {{type 'NonConformingStruct' does not conform to protocol 'Encodable'}}
  // expected-note@-4 {{did you mean 'init'?}}
  var w: NonCodable // expected-note {{cannot automatically synthesize 'Decodable' because 'NonCodable' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'NonCodable' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'NonCodable' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because 'NonCodable' does not conform to 'Encodable'}}
  var x: Int
  var y: Double
  static var z: String = "foo"

  // FIXME: Remove when conditional conformance lands.
  // Because conditional conformance is not yet available, Optional, Array,
  // Set, and Dictionary all conform to Codable even when their generic
  // parameters do not.
  // We want to make sure that these cases prevent derived conformance.
  var nonCodableOptional: NonCodable? = nil // expected-note {{cannot automatically synthesize 'Decodable' because 'NonCodable?' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'NonCodable?' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'NonCodable?' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because 'NonCodable?' does not conform to 'Encodable'}}
  var nonCodableArray: [NonCodable] = [] // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable]' does not conform to 'Encodable'}}
  var nonCodableSet: Set<NonCodable> = [] // expected-note {{cannot automatically synthesize 'Decodable' because 'Set<NonCodable>' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'Set<NonCodable>' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'Set<NonCodable>' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because 'Set<NonCodable>' does not conform to 'Encodable'}}
  var nonCodableDictionary1: [String : NonCodable] = [:] // expected-note {{cannot automatically synthesize 'Decodable' because '[String : NonCodable]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[String : NonCodable]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[String : NonCodable]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[String : NonCodable]' does not conform to 'Encodable'}}
  var nonCodableDictionary2: [NonCodable : String] = [:] // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : String]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : String]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : String]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : String]' does not conform to 'Encodable'}}
  var nonCodableDictionary3: [NonCodable : NonCodable] = [:] // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : NonCodable]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : NonCodable]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : NonCodable]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : NonCodable]' does not conform to 'Encodable'}}

  // These conditions should apply recursively, too.
  var nonCodableOptionalOptional: NonCodable?? = nil // expected-note {{cannot automatically synthesize 'Decodable' because 'NonCodable??' does not conform to 'Decodable'}}
    // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'NonCodable??' does not conform to 'Decodable'}}
    // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'NonCodable??' does not conform to 'Encodable'}}
    // expected-note@-3 {{cannot automatically synthesize 'Encodable' because 'NonCodable??' does not conform to 'Encodable'}}
  var nonCodableOptionalArray: [NonCodable]? = nil // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable]?' does not conform to 'Decodable'}}
    // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable]?' does not conform to 'Decodable'}}
    // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable]?' does not conform to 'Encodable'}}
    // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable]?' does not conform to 'Encodable'}}
  var nonCodableOptionalSet: Set<NonCodable>? = nil // expected-note {{cannot automatically synthesize 'Decodable' because 'Set<NonCodable>?' does not conform to 'Decodable'}}
    // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'Set<NonCodable>?' does not conform to 'Decodable'}}
    // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'Set<NonCodable>?' does not conform to 'Encodable'}}
    // expected-note@-3 {{cannot automatically synthesize 'Encodable' because 'Set<NonCodable>?' does not conform to 'Encodable'}}
  var nonCodableOptionalDictionary1: [String : NonCodable]? = nil // expected-note {{cannot automatically synthesize 'Decodable' because '[String : NonCodable]?' does not conform to 'Decodable'}}
    // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[String : NonCodable]?' does not conform to 'Decodable'}}
    // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[String : NonCodable]?' does not conform to 'Encodable'}}
    // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[String : NonCodable]?' does not conform to 'Encodable'}}
  var nonCodableOptionalDictionary2: [NonCodable : String]? = nil // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : String]?' does not conform to 'Decodable'}}
    // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : String]?' does not conform to 'Decodable'}}
    // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : String]?' does not conform to 'Encodable'}}
    // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : String]?' does not conform to 'Encodable'}}
  var nonCodableOptionalDictionary3: [NonCodable : NonCodable]? = nil // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : NonCodable]?' does not conform to 'Decodable'}}
    // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : NonCodable]?' does not conform to 'Decodable'}}
    // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : NonCodable]?' does not conform to 'Encodable'}}
    // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : NonCodable]?' does not conform to 'Encodable'}}

  var nonCodableArrayOptional: [NonCodable?] = [] // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable?]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable?]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable?]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable?]' does not conform to 'Encodable'}}
  var nonCodableArrayArray: [[NonCodable]] = [] // expected-note {{cannot automatically synthesize 'Decodable' because '[[NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[[NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[[NonCodable]]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[[NonCodable]]' does not conform to 'Encodable'}}
  var nonCodableArraySet: [Set<NonCodable>] = [] // expected-note {{cannot automatically synthesize 'Decodable' because '[Set<NonCodable>]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[Set<NonCodable>]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[Set<NonCodable>]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[Set<NonCodable>]' does not conform to 'Encodable'}}
  var nonCodableArrayDictionary1: [[String : NonCodable]] = [] // expected-note {{cannot automatically synthesize 'Decodable' because '[[String : NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[[String : NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[[String : NonCodable]]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[[String : NonCodable]]' does not conform to 'Encodable'}}
  var nonCodableArrayDictionary2: [[NonCodable : String]] = [] // expected-note {{cannot automatically synthesize 'Decodable' because '[[NonCodable : String]]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[[NonCodable : String]]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[[NonCodable : String]]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[[NonCodable : String]]' does not conform to 'Encodable'}}
  var nonCodableArrayDictionary3: [[NonCodable : NonCodable]] = [] // expected-note {{cannot automatically synthesize 'Decodable' because '[[NonCodable : NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[[NonCodable : NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[[NonCodable : NonCodable]]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[[NonCodable : NonCodable]]' does not conform to 'Encodable'}}

  var nonCodableDictionaryOptional1: [String : NonCodable?] = [:] // expected-note {{cannot automatically synthesize 'Decodable' because '[String : NonCodable?]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[String : NonCodable?]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[String : NonCodable?]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[String : NonCodable?]' does not conform to 'Encodable'}}
  var nonCodableDictionaryOptional2: [NonCodable : NonCodable?] = [:] // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : NonCodable?]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : NonCodable?]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : NonCodable?]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : NonCodable?]' does not conform to 'Encodable'}}
  var nonCodableDictionaryArray1: [String : [NonCodable]] = [:] // expected-note {{cannot automatically synthesize 'Decodable' because '[String : [NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[String : [NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[String : [NonCodable]]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[String : [NonCodable]]' does not conform to 'Encodable'}}
  var nonCodableDictionaryArray2: [NonCodable : [NonCodable]] = [:] // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : [NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : [NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : [NonCodable]]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : [NonCodable]]' does not conform to 'Encodable'}}
  var nonCodableDictionarySet1: [String : Set<NonCodable>] = [:] // expected-note {{cannot automatically synthesize 'Decodable' because '[String : Set<NonCodable>]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[String : Set<NonCodable>]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[String : Set<NonCodable>]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[String : Set<NonCodable>]' does not conform to 'Encodable'}}
  var nonCodableDictionarySet2: [NonCodable : Set<NonCodable>] = [:] // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : Set<NonCodable>]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : Set<NonCodable>]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : Set<NonCodable>]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : Set<NonCodable>]' does not conform to 'Encodable'}}
  var nonCodableDictionaryDictionary1: [String : [String : NonCodable]] = [:] // expected-note {{cannot automatically synthesize 'Decodable' because '[String : [String : NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[String : [String : NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[String : [String : NonCodable]]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[String : [String : NonCodable]]' does not conform to 'Encodable'}}
  var nonCodableDictionaryDictionary2: [NonCodable : [String : NonCodable]] = [:] // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : [String : NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : [String : NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : [String : NonCodable]]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : [String : NonCodable]]' does not conform to 'Encodable'}}
  var nonCodableDictionaryDictionary3: [String : [NonCodable : NonCodable]] = [:] // expected-note {{cannot automatically synthesize 'Decodable' because '[String : [NonCodable : NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[String : [NonCodable : NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[String : [NonCodable : NonCodable]]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[String : [NonCodable : NonCodable]]' does not conform to 'Encodable'}}
  var nonCodableDictionaryDictionary4: [NonCodable : [NonCodable : NonCodable]] = [:] // expected-note {{cannot automatically synthesize 'Decodable' because '[NonCodable : [NonCodable : NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because '[NonCodable : [NonCodable : NonCodable]]' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because '[NonCodable : [NonCodable : NonCodable]]' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because '[NonCodable : [NonCodable : NonCodable]]' does not conform to 'Encodable'}}

  // However, arbitrary generic types which _do_ conform to Codable should be
  // valid.
  var codableGenericThing1: CodableGeneric<NonCodable>? = nil
  var codableGenericThing2: CodableGeneric<NonCodable?>? = nil
  var codableGenericThing3: CodableGeneric<[NonCodable]>? = nil
  var codableGenericThing4: CodableGeneric<Set<NonCodable>>? = nil
  var codableGenericThing5: CodableGeneric<[String : NonCodable]>? = nil
  var codableGenericThing6: CodableGeneric<[NonCodable : String]>? = nil
  var codableGenericThing7: CodableGeneric<[NonCodable : NonCodable]>? = nil

  // These lines have to be within the NonConformingStruct type because
  // CodingKeys should be private.
  func foo() {
    // They should not get a CodingKeys type.
    let _ = NonConformingStruct.CodingKeys.self // expected-error {{type 'NonConformingStruct' has no member 'CodingKeys'}}
    let _ = NonConformingStruct.CodingKeys.x // expected-error {{type 'NonConformingStruct' has no member 'CodingKeys'}}
    let _ = NonConformingStruct.CodingKeys.y // expected-error {{type 'NonConformingStruct' has no member 'CodingKeys'}}
    let _ = NonConformingStruct.CodingKeys.z // expected-error {{type 'NonConformingStruct' has no member 'CodingKeys'}}
  }
}

// They should not receive Codable methods.
let _ = NonConformingStruct.init(from:) // expected-error {{type 'NonConformingStruct' has no member 'init(from:)'}}
let _ = NonConformingStruct.encode(to:) // expected-error {{type 'NonConformingStruct' has no member 'encode(to:)'}}

// They should not get a CodingKeys type.
let _ = NonConformingStruct.CodingKeys.self // expected-error {{type 'NonConformingStruct' has no member 'CodingKeys'}}
