// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// REQUIRES: objc_interop

import Foundation

// Structs with computed members should get synthesized conformance to Codable,
// but their lazy and computed members should be skipped as part of the
// synthesis.
struct StructWithComputedMembers : Codable {
    var x: Int
    lazy var y: Double = Double.pi
    var z: String {
        return "foo"
    }

    // These lines have to be within the StructWithComputedMembers type because
    // CodingKeys should be private.
    func foo() {
        // They should receive a synthesized CodingKeys enum.
        let _ = StructWithComputedMembers.CodingKeys.self

        // The enum should have a case for each of the vars.
        let _ = StructWithComputedMembers.CodingKeys.x

        // Lazy vars should not be part of the CodingKeys enum.
        let _ = StructWithComputedMembers.CodingKeys.y // expected-error {{type 'StructWithComputedMembers.CodingKeys' has no member 'y'}}

        // Computed vars should not be part of the CodingKeys enum.
        let _ = StructWithComputedMembers.CodingKeys.z // expected-error {{type 'StructWithComputedMembers.CodingKeys' has no member 'z'}}
    }
}

// They should receive synthesized init(from:) and an encode(to:).
let _ = StructWithComputedMembers.init(from:)
let _ = StructWithComputedMembers.encode(to:)

// The synthesized CodingKeys type should not be accessible from outside the
// struct.
let _ = StructWithComputedMembers.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
