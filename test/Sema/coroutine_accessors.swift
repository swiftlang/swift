// RUN: %target-typecheck-verify-swift \
// RUN:     -verify-additional-prefix enabled- \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -debug-diagnostic-names


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
                        // FIXME: Bad diagnostic!  The writer is non-mutating--the reader is mutating.
  nonmutating _modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when the setter is 'mutating'}}
                        // expected-note@-3{{setter defined here}}
    var fake: Int
    yield &fake
  }
}
var ingsn_m: Int {
  get { 0 }
  set {}
  nonmutating _modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when the setter is 'mutating'}}
                        // expected-note@-2{{setter defined here}}
    var fake: Int
    yield &fake
  }
}
var igsn_m: Int {
  mutating get { 0 }
  set {}
                        // TODO: Incomplete diagnostic!  Both the writer AND the reader are mutating.
  nonmutating _modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when the setter is 'mutating'}}
                        // expected-note@-3{{setter defined here}}
    var fake: Int
    yield &fake
  }
}
var ingns_m: Int {
  get { 0 }
  nonmutating set {}
  _modify { // expected-error{{'modify' accessor cannot be 'mutating' when the setter is 'nonmutating'}}
            // expected-note@-2{{setter defined here}}
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
                        // FIXME: Bad diagnostic!  The writer is non-mutating--the reader is mutating.
  nonmutating _modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when the setter is 'mutating'}}
                        // expected-note@-3{{setter defined here}}
    var fake: Int
    yield &fake
  }
}
var in_rsn_m: Int {
  _read { yield i }
  set {}
  nonmutating _modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when the setter is 'mutating'}}
                        // expected-note@-2{{setter defined here}}
    var fake: Int
    yield &fake
  }
}
var i_rsn_m: Int {
  mutating _read { yield i }
  set {}
                        // TODO: Incomplete diagnostic!  Both the writer AND the reader are mutating.
  nonmutating _modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when the setter is 'mutating'}}
                        // expected-note@-3{{setter defined here}}
    var fake: Int
    yield &fake
  }
}
var in_rns_m: Int {
  _read { yield i }
  nonmutating set {}
  _modify { // expected-error{{'modify' accessor cannot be 'mutating' when the setter is 'nonmutating'}}
            // expected-note@-2{{setter defined here}}
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
                        // FIXME: Bad diagnostic!  The writer is non-mutating--the reader is mutating.
  nonmutating _modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when the setter is 'mutating'}}
                        // expected-note@-3{{setter defined here}}
    var fake: Int
    yield &fake
  }
}
var inuasn_m: Int {
  unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  set {}
  nonmutating _modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when the setter is 'mutating'}}
                        // expected-note@-2{{setter defined here}}
    var fake: Int
    yield &fake
  }
}
var iuasn_m: Int {
  mutating unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  set {}
                        // TODO: Incomplete diagnostic!  Both the writer AND the reader are mutating.
  nonmutating _modify { // expected-error{{'modify' accessor cannot be 'nonmutating' when the setter is 'mutating'}}
                        // expected-note@-3{{setter defined here}}
    var fake: Int
    yield &fake
  }
}
var inuans_m: Int {
  unsafeAddress { UnsafePointer(bitPattern: 0x0)! }
  nonmutating set {}
  _modify { // expected-error{{'modify' accessor cannot be 'mutating' when the setter is 'nonmutating'}}
            // expected-note@-2{{setter defined here}}
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

}
