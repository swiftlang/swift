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

var iyb: Int {
  yielding borrow {
    fatalError()
  }
}

var ir: Int {
  read { // expected-error{{cannot_find_in_scope}}
    fatalError()
  }
}

var iybr: Int {
  yielding borrow {
    fatalError()
  }
  yielding borrow {
         // expected-error@-1{{duplicate_accessor}}
         // expected-note@-5{{previous_accessor}}
    fatalError()
  }
}

// conflicting accessors
var ir_r: Int {
  yielding borrow {
          // expected-error@-1{{variable cannot provide both a 'yielding borrow' accessor and a '_read' accessor}}
    fatalError()
  }
  _read { // expected-note{{previous_accessor}}
    fatalError()
  }
}

// conflicting accessors
var igr: Int {
  get {
    1
  }
  yielding borrow {
         // expected-error@-1{{variable cannot provide both a 'yielding borrow' accessor and a getter}}
         // expected-note@-5{{previous_accessor}}
    yield _i
  }
}

// =============================================================================
// One read, one write.
// =============================================================================

var igm: Int {
  get {
    0
  }
  yielding mutate {
    yield &_i
  }
}

var img: Int {
  yielding mutate {
    fatalError()
  }
  get {
    0
  }
}

var iuam: Int {
  unsafeAddress {
    UnsafePointer<Int>(bitPattern: 0x0)!
  }
  yielding mutate {
    yield &_i
  }
}

var imua: Int {
  yielding mutate {
    fatalError()
  }
  unsafeAddress {
    UnsafePointer<Int>(bitPattern: 0x0)!
  }
}

var i_rm: Int {
  _read {
    yield _i
  }
  yielding mutate {
    yield &_i
  }
}

var im_r: Int {
  yielding mutate {
    fatalError()
  }
  _read {
    fatalError()
  }
}

var irm: Int {
  yielding borrow {
    fatalError()
  }
  yielding mutate {
    fatalError()
  }
}

var imr: Int {
  yielding mutate {
    fatalError()
  }
  yielding borrow {
    fatalError()
  }
}

var irs: Int {
  yielding borrow {
    fatalError()
  }
  set {
  }
}

var isr: Int {
  set {
  }
  yielding borrow {
    fatalError()
  }
}

var iruma: Int {
  yielding borrow {
    fatalError()
  }
  unsafeMutableAddress {
    UnsafeMutablePointer<Int>(bitPattern: 0x0)!
  }
}

var iumar: Int {
  yielding borrow {
    fatalError()
  }
  unsafeMutableAddress {
    UnsafeMutablePointer<Int>(bitPattern: 0x0)!
  }
}

var ir_m: Int {
  yielding borrow {
    fatalError()
  }
  _modify {
    fatalError()
  }
}

var i_mr: Int {
  _modify {
    fatalError()
  }
  yielding borrow {
    fatalError()
  }
}

// =============================================================================
// Multiple mutating only.
// =============================================================================

// need a reader.
var ims: Int {
  yielding mutate {
    fatalError()
  }
  set { // expected-error{{variable with a setter must also have a getter, addressor, or 'yielding borrow' accessor}}
  }
}

// need a reader
var ism: Int {
  set { // expected-error{{variable with a setter must also have a getter, addressor, or 'yielding borrow' accessor}}
    fatalError()
  }
  yielding mutate {
    fatalError()
  }
}

// need a reader.
var imuma: Int {
  yielding mutate { // expected-error{{variable with a 'yielding mutate' accessor must also have a getter, addressor, or 'yielding borrow' accessor}}
    fatalError()
  }
  unsafeMutableAddress {
    UnsafeMutablePointer<Int>(bitPattern: 0x0)!
  }
}

// need a reader
var iumam: Int {
  unsafeMutableAddress {
    fatalError()
  }
  yielding mutate { // expected-error{{variable with a 'yielding mutate' accessor must also have a getter, addressor, or 'yielding borrow' accessor}}
    fatalError()
  }
}

// conflicting accessors.  need a reader.
var im_m: Int {
  yielding mutate { // expected-error{{variable cannot provide both a 'yielding mutate' accessor and a '_modify' accessor}}
                    // expected-note@+3{{'_modify' accessor defined here}}
    fatalError()
  }
  _modify { // expected-error{{variable with a '_modify' accessor must also have a getter, addressor, or 'yielding borrow' accessor}}
    fatalError()
  }
}

// need a reader.
var i_mm: Int {
  _modify { // expected-error{{variable with a '_modify' accessor must also have a getter, addressor, or 'yielding borrow' accessor}}
    fatalError()
  }
  yielding mutate {
            // expected-error@-1{{variable cannot provide both a 'yielding mutate' accessor and a '_modify' accessor}}
            // expected-note@-5{{previous_accessor}}
    fatalError()
  }
}

// need a reader.
var imm: Int {
  yielding mutate { // expected-error{{variable with a 'yielding mutate' accessor must also have a getter, addressor, or 'yielding borrow' accessor}}
    fatalError()
  }
  yielding mutate { // expected-error{{variable already has a 'yielding mutate' accessor}}
                    // expected-note@-4{{previous_accessor}}
    fatalError()
  }
}

// =============================================================================
// Multiple
// =============================================================================

// duplicate accessors
var i_rm_m: Int {
  _read {
    yield _i
  }
  yielding mutate {
           // expected-error@-1{{variable cannot provide both a 'yielding mutate' accessor and a '_modify' accessor}}
    yield &_i
  }
  _modify { // expected-note{{previous_accessor}}
    yield &_i
  }
}

// duplicate accessors
var ir_rm_m: Int {
  yielding borrow { // expected-error{{variable cannot provide both a 'yielding borrow' accessor and a '_read' accessor}}
    fatalError()
  }
  _read { // expected-note{{previous_accessor}}
    fatalError()
  }
  yielding mutate { // expected-error{{variable cannot provide both a 'yielding mutate' accessor and a '_modify' accessor}}
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
  var goodP: Int { yielding borrow set }

  var badP: Int { yielding borrow yielding mutate }
  //expected-error@-1{{expected 'get', 'yielding borrow', or 'set' in a protocol property}}

  subscript(goodS goodS: Int) -> Int { yielding borrow set }

  subscript(badS badS: Int) -> Int { yielding borrow yielding mutate }
  //expected-error@-1{{expected 'get', 'yielding borrow', or 'set' in a protocol property}}

}
