// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop

import TypedUntypedEnums

let _: Set = [kBlue, kGreen]        // construct a set from blue and green
let _: Set = [kBlue, kBlue]         // construct a valid set from two blues

// Not allowed to mix and match elements of a set.
let _: Set = [kBlue, kTwo]          // expected-error {{conflicting arguments to generic parameter 'Element' ('Int' vs. 'Color')}}
let _: Set = [kBlue, Pet.dogcow]    // expected-error {{conflicting arguments to generic parameter 'Element' ('Pet' vs. 'Color')}}

let s: Set<Pet> = []                // construct an empty, type-annotated set
let _ = s.contains(Pet.goat)        // query the empty set using a key
let _ = s.contains(kTwo)            // expected-error {{cannot convert value of type 'Int' to expected argument type 'Pet'}}

// Untyped enums can be used interchangeably with integers
let _: Set = [kFour, kTwo]          // construct a set from untyped enum
let _: Set = [kFour, kTwo, 0]       // construct a set that mixes untyped enums and integers

let _ = [Pet.goat: "baa", Pet.dogcow: "moo"]                        // dictionaries are fine too
let _: [AnyHashable: String] = [Pet.goat: "baa", kFour: "meow"]     // even heterogeneous ones
