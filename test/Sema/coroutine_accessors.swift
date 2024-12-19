// RUN: %target-typecheck-verify-swift \
// RUN:     -verify-additional-prefix enabled- \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -debug-diagnostic-names

// REQUIRES: swift_feature_CoroutineAccessors

struct S {
var i: Int

// Get+Set+_Modify

// +---+---+---+
// | g | s |_m |
// +---+---+---+
// | n | n | n | ok  ( ingnsn_m )
// | y | n | n | bad ( ignsn_m )
// | n | y | n | bad ( ingsn_m )
// | y | y | n | bad ( igsn_m )
// | n | n | y | bad ( ingns_m )
// | y | n | y | ok  ( igns_m )
// | n | y | y | ok  ( ings_m )
// | y | y | y | ok  ( igs_m )
// +---+---+---+

var ingnsn_m: Int {
  get { 0 }
  nonmutating set {}
  nonmutating _modify {
    var fake: Int
    yield &fake
  }
}
var ignsn_m: Int {
  mutating get { 0 }
  nonmutating set {}
  nonmutating _modify { // expected-error{{'_modify' accessor cannot be 'nonmutating' when the getter is 'mutating'}}
                        // expected-note@-3{{getter defined here}}
    var fake: Int
    yield &fake
  }
}
var ingsn_m: Int {
  get { 0 }
  set {}
  nonmutating _modify { // expected-error{{'_modify' accessor cannot be 'nonmutating' when the setter is not 'nonmutating'}}
                        // expected-note@-2{{setter defined here}}
    var fake: Int
    yield &fake
  }
}
var igsn_m: Int {
  mutating get { 0 }
  set {}
  nonmutating _modify { // expected-error{{'_modify' accessor cannot be 'nonmutating' when either the setter is not 'nonmutating' or the getter is 'mutating'}}
                        // expected-note@-2{{setter defined here}}
                        // expected-note@-4{{getter defined here}}
    var fake: Int
    yield &fake
  }
}
var ingns_m: Int {
  get { 0 }
  nonmutating set {}
  _modify { // expected-error{{'_modify' accessor cannot be 'mutating' when both the setter is 'nonmutating' and the getter is not 'mutating'}}
            // expected-note@-2{{setter defined here}}
            // expected-note@-4{{getter defined here}}
    yield &i
  }
}
var igns_m: Int {
  mutating get { 0 }
  nonmutating set {}
  _modify {
    yield &i
  }
}
var ings_m: Int {
  get { 0 }
  set {}
  _modify {
    yield &i
  }
}
var igs_m: Int {
  mutating get { 0 }
  nonmutating set {}
  _modify {
    yield &i
  }
}

// _Read+Set+_Modify

// +---+---+---+
// | _r | s |_m |
// +---+---+---+
// | n | n | n | ok  ( in_rnsn_m )
// | y | n | n | bad ( i_rnsn_m )
// | n | y | n | bad ( in_rsn_m )
// | y | y | n | bad ( i_rsn_m )
// | n | n | y | bad ( in_rns_m )
// | y | n | y | ok  ( i_rns_m )
// | n | y | y | ok  ( in_rs_m )
// | y | y | y | ok  ( i_rs_m )
// +---+---+---+

var in_rnsn_m: Int {
  _read { yield i }
  nonmutating set {}
  nonmutating _modify {
    var fake: Int
    yield &fake
  }
}
var i_rnsn_m: Int {
  mutating _read { yield i }
  nonmutating set {}
  nonmutating _modify { // expected-error{{'_modify' accessor cannot be 'nonmutating' when the '_read' accessor is 'mutating'}}
                        // expected-note@-3{{'_read' accessor defined here}}
    var fake: Int
    yield &fake
  }
}
var in_rsn_m: Int {
  _read { yield i }
  set {}
  nonmutating _modify { // expected-error{{'_modify' accessor cannot be 'nonmutating' when the setter is not 'nonmutating'}}
                        // expected-note@-2{{setter defined here}}
    var fake: Int
    yield &fake
  }
}
var i_rsn_m: Int {
  mutating _read { yield i }
  set {}
  nonmutating _modify { // expected-error{{'_modify' accessor cannot be 'nonmutating' when either the setter is not 'nonmutating' or the '_read' accessor is 'mutating'}}
                        // expected-note@-2{{setter defined here}}
                        // expected-note@-4{{'_read' accessor defined here}}
    var fake: Int
    yield &fake
  }
}
var in_rns_m: Int {
  _read { yield i }
  nonmutating set {}
  _modify { // expected-error{{'_modify' accessor cannot be 'mutating' when both the setter is 'nonmutating' and the '_read' accessor is not 'mutating'}}
            // expected-note@-2{{setter defined here}}
            // expected-note@-4{{'_read' accessor defined here}}
    yield &i
  }
}
var i_rns_m: Int {
  mutating _read { yield i }
  nonmutating set {}
  _modify {
    yield &i
  }
}
var in_rs_m: Int {
  _read { yield i }
  set {}
  _modify {
    yield &i
  }
}
var i_rs_m: Int {
  mutating _read { yield i }
  nonmutating set {}
  _modify {
    yield &i
  }
}

// Read+Set+_Modify

// +---+---+---+
// | r | s |_m |
// +---+---+---+
// | n | n | n | ok  ( inrnsn_m )
// | y | n | n | bad ( irnsn_m )
// | n | y | n | bad ( inrsn_m )
// | y | y | n | bad ( irsn_m )
// | n | n | y | bad ( inrns_m )
// | y | n | y | ok  ( irns_m )
// | n | y | y | ok  ( inrs_m )
// | y | y | y | ok  ( irs_m )
// +---+---+---+

var inrnsn_m: Int {
  read { yield i }
  nonmutating set {}
  nonmutating _modify {
    var fake: Int
    yield &fake
  }
}
var irnsn_m: Int {
  mutating read { yield i }
  nonmutating set {}
  nonmutating _modify { // expected-error{{'_modify' accessor cannot be 'nonmutating' when the 'read' accessor is 'mutating'}}
                        // expected-note@-3{{'read' accessor defined here}}
    var fake: Int
    yield &fake
  }
}
var inrsn_m: Int {
  read { yield i }
  set {}
  nonmutating _modify { // expected-error{{'_modify' accessor cannot be 'nonmutating' when the setter is not 'nonmutating'}}
                        // expected-note@-2{{setter defined here}}
    var fake: Int
    yield &fake
  }
}
var irsn_m: Int {
  mutating read { yield i }
  set {}
  nonmutating _modify { // expected-error{{'_modify' accessor cannot be 'nonmutating' when either the setter is not 'nonmutating' or the 'read' accessor is 'mutating'}}
                        // expected-note@-2{{setter defined here}}
                        // expected-note@-4{{'read' accessor defined here}}
    var fake: Int
    yield &fake
  }
}
var inrns_m: Int {
  read { yield i }
  nonmutating set {}
  _modify { // expected-error{{'_modify' accessor cannot be 'mutating' when both the setter is 'nonmutating' and the 'read' accessor is not 'mutating'}}
            // expected-note@-2{{setter defined here}}
            // expected-note@-4{{'read' accessor defined here}}
    yield &i
  }
}
var irns_m: Int {
  mutating read { yield i }
  nonmutating set {}
  _modify {
    yield &i
  }
}
var inrs_m: Int {
  read { yield i }
  set {}
  _modify {
    yield &i
  }
}
var irs_m: Int {
  mutating read { yield i }
  nonmutating set {}
  _modify {
    yield &i
  }
}

// UnsafeAddress+Set+_Modify

// +---+---+---+
// |ua | s |_m |
// +---+---+---+
// | n | n | n | ok  ( inuansn_m )
// | y | n | n | bad ( iuansn_m )
// | n | y | n | bad ( inuasn_m )
// | y | y | n | bad ( iuasn_m )
// | n | n | y | bad ( inuans_m )
// | y | n | y | ok  ( iuans_m )
// | n | y | y | ok  ( inuas_m )
// | y | y | y | ok  ( iuas_m )
// +---+---+---+

var inuansn_m: Int {
  unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  nonmutating set {}
  nonmutating _modify {
    var fake: Int
    yield &fake
  }
}
var iuansn_m: Int {
  mutating unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  nonmutating set {}
  nonmutating _modify { // expected-error{{'_modify' accessor cannot be 'nonmutating' when the addressor is 'mutating'}}
                        // expected-note@-3{{addressor defined here}}
    var fake: Int
    yield &fake
  }
}
var inuasn_m: Int {
  unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  set {}
  nonmutating _modify { // expected-error{{'_modify' accessor cannot be 'nonmutating' when the setter is not 'nonmutating'}}
                        // expected-note@-2{{setter defined here}}
    var fake: Int
    yield &fake
  }
}
var iuasn_m: Int {
  mutating unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  set {}
  nonmutating _modify { // expected-error{{'_modify' accessor cannot be 'nonmutating' when either the setter is not 'nonmutating' or the addressor is 'mutating'}}
                        // expected-note@-2{{setter defined here}}
                        // expected-note@-4{{addressor defined here}}
    var fake: Int
    yield &fake
  }
}
var inuans_m: Int {
  unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  nonmutating set {}
  _modify { // expected-error{{'_modify' accessor cannot be 'mutating' when both the setter is 'nonmutating' and the addressor is not 'mutating'}}
            // expected-note@-2{{setter defined here}}
            // expected-note@-4{{addressor defined here}}
    yield &i
  }
}
var iuans_m: Int {
  mutating unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  nonmutating set {}
  _modify {
    yield &i
  }
}
var inuas_m: Int {
  unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  set {}
  _modify {
    yield &i
  }
}
var iuas_m: Int {
  mutating unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  nonmutating set {}
  _modify {
    yield &i
  }
}

// Get+Set+Modify

// +---+---+---+
// | g | s |m |
// +---+---+---+
// | n | n | n | ok  ( ingnsnm )
// | y | n | n | bad ( ignsnm )
// | n | y | n | bad ( ingsnm )
// | y | y | n | bad ( igsnm )
// | n | n | y | bad ( ingnsm )
// | y | n | y | ok  ( ignsm )
// | n | y | y | ok  ( ingsm )
// | y | y | y | ok  ( igsm )
// +---+---+---+

var ingnsnm: Int {
  get { 0 }
  nonmutating set {}
  nonmutating modify {
    var fake: Int
    yield &fake
  }
}
var ignsnm: Int {
  mutating get { 0 }
  nonmutating set {}
  nonmutating modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when the getter is 'mutating'}}
                        // expected-note@-3{{getter defined here}}
    var fake: Int
    yield &fake
  }
}
var ingsnm: Int {
  get { 0 }
  set {}
  nonmutating modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when the setter is not 'nonmutating'}}
                        // expected-note@-2{{setter defined here}}
    var fake: Int
    yield &fake
  }
}
var igsnm: Int {
  mutating get { 0 }
  set {}
  nonmutating modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when either the setter is not 'nonmutating' or the getter is 'mutating'}}
                        // expected-note@-2{{setter defined here}}
                        // expected-note@-4{{getter defined here}}
    var fake: Int
    yield &fake
  }
}
var ingnsm: Int {
  get { 0 }
  nonmutating set {}
  modify { // expected-error{{'modify' accessor cannot be 'mutating' when both the setter is 'nonmutating' and the getter is not 'mutating'}}
            // expected-note@-2{{setter defined here}}
            // expected-note@-4{{getter defined here}}
    yield &i
  }
}
var ignsm: Int {
  mutating get { 0 }
  nonmutating set {}
  modify {
    yield &i
  }
}
var ingsm: Int {
  get { 0 }
  set {}
  modify {
    yield &i
  }
}
var igsm: Int {
  mutating get { 0 }
  nonmutating set {}
  modify {
    yield &i
  }
}

// _Read+Set+Modify

// +---+---+---+
// | _r | s |m |
// +---+---+---+
// | n | n | n | ok  ( in_rnsnm )
// | y | n | n | bad ( i_rnsnm )
// | n | y | n | bad ( in_rsnm )
// | y | y | n | bad ( i_rsnm )
// | n | n | y | bad ( in_rnsm )
// | y | n | y | ok  ( i_rnsm )
// | n | y | y | ok  ( in_rsm )
// | y | y | y | ok  ( i_rsm )
// +---+---+---+

var in_rnsnm: Int {
  _read { yield i }
  nonmutating set {}
  nonmutating modify {
    var fake: Int
    yield &fake
  }
}
var i_rnsnm: Int {
  mutating _read { yield i }
  nonmutating set {}
  nonmutating modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when the '_read' accessor is 'mutating'}}
                        // expected-note@-3{{'_read' accessor defined here}}
    var fake: Int
    yield &fake
  }
}
var in_rsnm: Int {
  _read { yield i }
  set {}
  nonmutating modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when the setter is not 'nonmutating'}}
                        // expected-note@-2{{setter defined here}}
    var fake: Int
    yield &fake
  }
}
var i_rsnm: Int {
  mutating _read { yield i }
  set {}
  nonmutating modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when either the setter is not 'nonmutating' or the '_read' accessor is 'mutating'}}
                        // expected-note@-2{{setter defined here}}
                        // expected-note@-4{{'_read' accessor defined here}}
    var fake: Int
    yield &fake
  }
}
var in_rnsm: Int {
  _read { yield i }
  nonmutating set {}
  modify { // expected-error{{'modify' accessor cannot be 'mutating' when both the setter is 'nonmutating' and the '_read' accessor is not 'mutating'}}
            // expected-note@-2{{setter defined here}}
            // expected-note@-4{{'_read' accessor defined here}}
    yield &i
  }
}
var i_rnsm: Int {
  mutating _read { yield i }
  nonmutating set {}
  modify {
    yield &i
  }
}
var in_rsm: Int {
  _read { yield i }
  set {}
  modify {
    yield &i
  }
}
var i_rsm: Int {
  mutating _read { yield i }
  nonmutating set {}
  modify {
    yield &i
  }
}

// Read+Set+Modify

// +---+---+---+
// | r | s |m |
// +---+---+---+
// | n | n | n | ok  ( inrnsnm )
// | y | n | n | bad ( irnsnm )
// | n | y | n | bad ( inrsnm )
// | y | y | n | bad ( irsnm )
// | n | n | y | bad ( inrnsm )
// | y | n | y | ok  ( irnsm )
// | n | y | y | ok  ( inrsm )
// | y | y | y | ok  ( irsm )
// +---+---+---+

var inrnsnm: Int {
  read { yield i }
  nonmutating set {}
  nonmutating modify {
    var fake: Int
    yield &fake
  }
}
var irnsnm: Int {
  mutating read { yield i }
  nonmutating set {}
  nonmutating modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when the 'read' accessor is 'mutating'}}
                        // expected-note@-3{{'read' accessor defined here}}
    var fake: Int
    yield &fake
  }
}
var inrsnm: Int {
  read { yield i }
  set {}
  nonmutating modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when the setter is not 'nonmutating'}}
                        // expected-note@-2{{setter defined here}}
    var fake: Int
    yield &fake
  }
}
var irsnm: Int {
  mutating read { yield i }
  set {}
  nonmutating modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when either the setter is not 'nonmutating' or the 'read' accessor is 'mutating'}}
                        // expected-note@-2{{setter defined here}}
                        // expected-note@-4{{'read' accessor defined here}}
    var fake: Int
    yield &fake
  }
}
var inrnsm: Int {
  read { yield i }
  nonmutating set {}
  modify { // expected-error{{'modify' accessor cannot be 'mutating' when both the setter is 'nonmutating' and the 'read' accessor is not 'mutating'}}
            // expected-note@-2{{setter defined here}}
            // expected-note@-4{{'read' accessor defined here}}
    yield &i
  }
}
var irnsm: Int {
  mutating read { yield i }
  nonmutating set {}
  modify {
    yield &i
  }
}
var inrsm: Int {
  read { yield i }
  set {}
  modify {
    yield &i
  }
}
var irsm: Int {
  mutating read { yield i }
  nonmutating set {}
  modify {
    yield &i
  }
}

// UnsafeAddress+Set+Modify

// +---+---+---+
// |ua | s |m |
// +---+---+---+
// | n | n | n | ok  ( inuansnm )
// | y | n | n | bad ( iuansnm )
// | n | y | n | bad ( inuasnm )
// | y | y | n | bad ( iuasnm )
// | n | n | y | bad ( inuansm )
// | y | n | y | ok  ( iuansm )
// | n | y | y | ok  ( inuasm )
// | y | y | y | ok  ( iuasm )
// +---+---+---+

var inuansnm: Int {
  unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  nonmutating set {}
  nonmutating modify {
    var fake: Int
    yield &fake
  }
}
var iuansnm: Int {
  mutating unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  nonmutating set {}
  nonmutating modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when the addressor is 'mutating'}}
                        // expected-note@-3{{addressor defined here}}
    var fake: Int
    yield &fake
  }
}
var inuasnm: Int {
  unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  set {}
  nonmutating modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when the setter is not 'nonmutating'}}
                        // expected-note@-2{{setter defined here}}
    var fake: Int
    yield &fake
  }
}
var iuasnm: Int {
  mutating unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  set {}
  nonmutating modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when either the setter is not 'nonmutating' or the addressor is 'mutating'}}
                        // expected-note@-2{{setter defined here}}
                        // expected-note@-4{{addressor defined here}}
    var fake: Int
    yield &fake
  }
}
var inuansm: Int {
  unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  nonmutating set {}
  modify { // expected-error{{'modify' accessor cannot be 'mutating' when both the setter is 'nonmutating' and the addressor is not 'mutating'}}
            // expected-note@-2{{setter defined here}}
            // expected-note@-4{{addressor defined here}}
    yield &i
  }
}
var iuansm: Int {
  mutating unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  nonmutating set {}
  modify {
    yield &i
  }
}
var inuasm: Int {
  unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  set {}
  modify {
    yield &i
  }
}
var iuasm: Int {
  mutating unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  nonmutating set {}
  modify {
    yield &i
  }
}

}
