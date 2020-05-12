// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-cxx-interop

import MemberwiseInitializer

let structPrivateOnly = StructPrivateOnly(varPrivate: 42)  // expected-error {{argument passed to call that takes no arguments}}
let structPublicOnly = StructPublicOnly(varPublic: 42)
let structEmptyPrivateSetion = StructEmptyPrivateSection(varPublic: 42)
let structPublicAndPrivate1 = StructPublicAndPrivate(varPublic: 42)  // expected-error {{argument passed to call that takes no arguments}}
let structPublicAndPrivate2 = StructPublicAndPrivate(varPublic: 42, varPrivate: 23)  // expected-error {{argument passed to call that takes no arguments}}
let structWithUnimportedMemberFunction = StructWithUnimportedMemberFunction(varPublic: 42)

let classPrivateOnly = ClassPrivateOnly(varPrivate: 42) // expected-error {{argument passed to call that takes no arguments}}
let classPublicOnly = ClassPublicOnly(varPublic: 42)
let classEmptyPublicSetion = ClassEmptyPublicSection(varPrivate: 42) // expected-error {{argument passed to call that takes no arguments}}
let classPublicAndPrivate1 = ClassPrivateAndPublic(varPublic: 23)  // expected-error {{argument passed to call that takes no arguments}}
let classPublicAndPrivate2 = ClassPrivateAndPublic(varPrivate: 42, varPublic: 23)  // expected-error {{argument passed to call that takes no arguments}}
let classWithUnimportedMemberFunction = ClassWithUnimportedMemberFunction(varPublic: 42)
