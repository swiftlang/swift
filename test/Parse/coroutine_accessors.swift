// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated \
// RUN:     -verify-additional-prefix enabled- \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -debug-diagnostic-names
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated \
// RUN:     -verify-additional-prefix disabled- \
// RUN:     -debug-diagnostic-names

// REQUIRES: swift_feature_CoroutineAccessors

var _i: Int = 0

// Order of accessor kinds:
// readers:
// - get
// - unsafeAddress
// - _read
// - read
// - yielding borrow
// writers:
// - set
// - unsafeMutableAddress
// - _modify
// - modify
// - yielding mutate

// =============================================================================
// Multiple reads
// =============================================================================

// If feature is disabled, this looks like an implicit
// getter that starts with an expression referring to
// symbols `yielding` and `borrow`
var iyb: Int {
  yielding borrow { // expected-disabled-error{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
}

var ir: Int {
  read { // expected-enabled-warning{{old_coroutine_accessor}}
    // expected-disabled-error@-1{{cannot_find_in_scope}}
    fatalError()
  }
}

// enabled: conflicting accessors
// disabled: implicit getter
var iybr: Int {
  yielding borrow { // expected-disabled-error{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
  read { // expected-enabled-warning{{old_coroutine_accessor}}
         // expected-enabled-error@-1{{duplicate_accessor}}
         // expected-enabled-note@-5{{previous_accessor}}
         // expected-disabled-error@-3{{expected_accessor_kw}}
    fatalError()
  }
}

// enabled: conflicting accessors
// disabled: implicit getter
var iryb: Int {
  read {  // expected-enabled-warning{{old_coroutine_accessor}}
    // expected-disabled-error@-1{{cannot_find_in_scope}}
    fatalError()
  }
  yielding borrow { // expected-enabled-error{{duplicate_accessor}}
      // expected-enabled-note@-5{{previous_accessor}}
      // expected-disabled-error@-2{{cannot_find_in_scope}}
      // expected-disabled-error@-3{{cannot_find_in_scope}}
      // expected-disabled-error@-4{{statement_same_line_without_semi}}
    fatalError()
  }
}

// enabled: conflicting accessors
// disabled: implicit getter.
var ir_r: Int {
  read {  // expected-enabled-warning{{old_coroutine_accessor}}
          // expected-enabled-error@-1{{variable cannot provide both a 'yielding borrow' accessor and a '_read' accessor}}
          // expected-disabled-error@-2{{cannot_find_in_scope}}
    fatalError()
  }
  _read { // expected-disabled-error{{cannot_find_in_scope}}
         // expected-enabled-note@-1{{previous_accessor}}
    fatalError()
  }
}

// enabled: conflicting accessors
var igr: Int {
  get {
    1
  }
  read { // expected-enabled-warning{{old_coroutine_accessor}}
         // expected-enabled-error@-1{{variable cannot provide both a 'yielding borrow' accessor and a getter}}
         // expected-enabled-note@-5{{previous_accessor}}
         // expected-disabled-error@-3{{expected_accessor_kw}}
    yield _i
  }
}

// =============================================================================
// One read, one write.
// =============================================================================

// enabled: ok
// disabled: bad keyword
var igm: Int {
  get {
    0
  }
  yielding mutate { // expected-disabled-error{{'yielding mutate' accessor is only valid when experimental feature coroutine accessors is enabled}}
    yield &_i
  }
}

// enabled: ok
// disabled: implicit getter
var img: Int {
  yielding mutate { // expected-disabled-error{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
  get {
    0
  }
}

// enabled: ok
// disabled: bad keyword
var iuam: Int {
  unsafeAddress {
    UnsafePointer<Int>(bitPattern: 0x0)!
  }
  yielding mutate { // expected-disabled-error{{'yielding mutate' accessor is only valid when experimental feature coroutine accessors is enabled}}
    yield &_i
  }
}

// enabled: ok
// disabled: bad keyword
var imua: Int {
  yielding mutate { // expected-disabled-error{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
  unsafeAddress {
    UnsafePointer<Int>(bitPattern: 0x0)!
  }
}

// enabled: ok
// disabled: bad keyword
var i_rm: Int {
  _read {
    yield _i
  }
  yielding mutate { // expected-disabled-error{{'yielding mutate' accessor is only valid when experimental feature coroutine accessors is enabled}}
    yield &_i
  }
}

// enabled: ok
// disabled: implicit getter.
var im_r: Int {
  yielding mutate { // expected-disabled-error{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
  _read {
    fatalError()
  }
}

// enabled: ok
// disabled: implicit getter
var irm: Int {
  yielding borrow { // expected-disabled-error{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
  yielding mutate { // expected-disabled-error{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
}

// enabled: ok
// disabled: implicit getter.
var imr: Int {
  yielding mutate { // expected-disabled-error{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
  yielding borrow { // expected-disabled-error{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
}

// enabled: ok
// disabled: implicit getter
var irs: Int {
  yielding borrow { // expected-disabled-error{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
  set {
  }
}

// enabled: ok
// disabled: bad keyword
var isr: Int {
  set {
  }
  yielding borrow { // expected-disabled-error{{'yielding borrow' accessor is only valid when experimental feature coroutine accessors is enabled}}
    fatalError()
  }
}

// enabled: ok
// disabled: implicit getter.
var iruma: Int {
  yielding borrow { // expected-disabled-error{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
  unsafeMutableAddress {
    UnsafeMutablePointer<Int>(bitPattern: 0x0)!
  }
}

// enabled: ok
// disabled: implicit getter.
var iumar: Int {
  yielding borrow { // expected-disabled-error{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
  unsafeMutableAddress {
    UnsafeMutablePointer<Int>(bitPattern: 0x0)!
  }
}

// enabled: ok
// disabled: implicit getter
var ir_m: Int {
  yielding borrow { // expected-disabled-error{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
  _modify {
    fatalError()
  }
}

// enabled: ok
// disabled: bad keyword
var i_mr: Int {
  _modify {
    fatalError()
  }
  yielding borrow { // expected-disabled-error{{'yielding borrow' accessor is only valid when experimental feature coroutine accessors is enabled}}
    fatalError()
  }
}

// =============================================================================
// Multiple mutating only.
// =============================================================================

// enabled: need a reader.
// disabled: implicit getter.
var ims: Int {
  yielding mutate { // expected-disabled-error{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
  set { // expected-error{{variable with a setter must also have a getter, addressor, or 'yielding borrow' accessor}}
  }
}

// enabled: need a reader
// disabled: bad keyword
var ism: Int {
  set { // expected-error{{variable with a setter must also have a getter, addressor, or 'yielding borrow' accessor}}
    fatalError()
  }
  yielding mutate {// expected-disabled-error{{'yielding mutate' accessor is only valid when experimental feature coroutine accessors is enabled}}
    fatalError()
  }
}

// enabled: need a reader.
// disabled: implicit getter.
var imuma: Int {
  yielding mutate { // expected-error{{variable with a 'yielding mutate' accessor must also have a getter, addressor, or 'yielding borrow' accessor}}
                   // expected-disabled-error@-1{{accessor_requires_coroutine_accessors}}

    fatalError()
  }
  unsafeMutableAddress {
    UnsafeMutablePointer<Int>(bitPattern: 0x0)!
  }
}

// enabled: need a reader
// disabled: bad keyword
var iumam: Int {
  unsafeMutableAddress {
    fatalError()
  }
  yielding mutate { // expected-error{{variable with a 'yielding mutate' accessor must also have a getter, addressor, or 'yielding borrow' accessor}}
           // expected-disabled-error@-1{{'yielding mutate' accessor is only valid when experimental feature coroutine accessors is enabled}}
    fatalError()
  }
}

// enabled: conflicting accessors.  need a reader.
// disabled: implicit getter.
var im_m: Int {
  yielding mutate { // expected-error{{variable cannot provide both a 'yielding mutate' accessor and a '_modify' accessor}}
                    // expected-note@+4{{'_modify' accessor defined here}}
                    // expected-disabled-error@-2{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
  _modify { // expected-error{{variable with a '_modify' accessor must also have a getter, addressor, or 'yielding borrow' accessor}}
    fatalError()
  }
}

// enabled: need a reader.
// disabled: implicit getter.
var i_mm: Int {
  _modify { // expected-error{{variable with a '_modify' accessor must also have a getter, addressor, or 'yielding borrow' accessor}}
    fatalError()
  }
  yielding mutate { // expected-disabled-error{{'yielding mutate' accessor is only valid when experimental feature coroutine accessors is enabled}}
            // expected-error@-1{{variable cannot provide both a 'yielding mutate' accessor and a '_modify' accessor}}
            // expected-note@-5{{previous_accessor}}
    fatalError()
  }
}

// enabled: need a reader.
// disabled: implicit getter.
var imm: Int {
  yielding mutate { // expected-error{{variable with a 'yielding mutate' accessor must also have a getter, addressor, or 'yielding borrow' accessor}}
                    // expected-disabled-error@-1{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
  yielding mutate { // expected-error{{variable already has a 'yielding mutate' accessor}}
                    // expected-note@-5{{previous_accessor}}
                    // expected-disabled-error@-2{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
}

// =============================================================================
// Multiple
// =============================================================================

// enabled: ok
// disabled: bad keyword
var i_rm_m: Int {
  _read {
    yield _i
  }
  yielding mutate { // expected-disabled-error{{'yielding mutate' accessor is only valid when experimental feature coroutine accessors is enabled}}
           // expected-error@-1{{variable cannot provide both a 'yielding mutate' accessor and a '_modify' accessor}}
    yield &_i
  }
  _modify { // expected-note{{previous_accessor}}
    yield &_i
  }
}

// enabled: ok
// disabled: implicit getter
var ir_rm_m: Int {
  yielding borrow { // expected-error{{variable cannot provide both a 'yielding borrow' accessor and a '_read' accessor}}
        // expected-disabled-error@-1{{accessor_requires_coroutine_accessors}}

    fatalError()
  }
  _read { // expected-note{{previous_accessor}}
    fatalError()
  }
  yielding mutate { // expected-error{{variable cannot provide both a 'yielding mutate' accessor and a '_modify' accessor}}
         // expected-disabled-error@-1{{accessor_requires_coroutine_accessors}}
    fatalError()
  }
  _modify { // expected-note{{previous_accessor}}
    fatalError()
  }
}

// =============================================================================
// Protocol Requirements
// Note that `yielding mutate` is not allowed as a protocol requirement
// =============================================================================

protocol P {
  var goodP: Int { yielding borrow set } //expected-disabled-error{{accessor_requires_coroutine_accessors}}

  var badP: Int { yielding borrow yielding mutate } //expected-disabled-error{{accessor_requires_coroutine_accessors}}
                                                    //expected-error@-1{{expected 'get', 'yielding borrow', or 'set' in a protocol property}}

  subscript(goodS goodS: Int) -> Int { yielding borrow set } //expected-disabled-error{{accessor_requires_coroutine_accessors}}

  subscript(badS badS: Int) -> Int { yielding borrow yielding mutate }  //expected-disabled-error{{accessor_requires_coroutine_accessors}}
                                                    //expected-error@-1{{expected 'get', 'yielding borrow', or 'set' in a protocol property}}

}
