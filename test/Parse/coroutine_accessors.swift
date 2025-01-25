// RUN: %target-typecheck-verify-swift \
// RUN:     -verify-additional-prefix enabled- \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -debug-diagnostic-names
// RUN: %target-typecheck-verify-swift \
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
// writers:
// - set
// - unsafeMutableAddress
// - _modify
// - modify

// =============================================================================
// Two reads
// =============================================================================

// enabled: conflicting accessors
// disabled: implicit getter.
var ir_r: Int {
  read { // expected-disabled-error{{cannot_find_in_scope}}
         // expected-enabled-error@-1{{variable cannot provide both a 'read' accessor and a '_read' accessor}}
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
  read { // expected-error{{variable cannot provide both a 'read' accessor and a getter}}
         // expected-note@-4{{previous_accessor}}
         // expected-disabled-error@-2{{'read' accessor is only valid when experimental feature coroutine accessors is enabled}}
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
  modify { // expected-disabled-error{{'modify' accessor is only valid when experimental feature coroutine accessors is enabled}}
    yield &_i
  }
}

// enabled: ok
// disabled: implicit getter
var img: Int {
  modify { // expected-disabled-error{{cannot_find_in_scope}}
    fatalError()
  }
  get { // expected-disabled-error{{cannot_find_in_scope}}
    0
  }
}

// enabled: ok
// disabled: bad keyword
var iuam: Int {
  unsafeAddress {
    UnsafePointer<Int>(bitPattern: 0x0)!
  }
  modify { // expected-disabled-error{{'modify' accessor is only valid when experimental feature coroutine accessors is enabled}}
    yield &_i
  }
}

// enabled: ok
// disabled: bad keyword
var imua: Int {
  modify { // expected-disabled-error{{cannot_find_in_scope}}
    fatalError()
  }
  unsafeAddress { // expected-disabled-error{{cannot_find_in_scope}}
    UnsafePointer<Int>(bitPattern: 0x0)!
  }
}

// enabled: ok
// disabled: bad keyword
var i_rm: Int {
  _read {
    yield _i
  }
  modify { // expected-disabled-error{{'modify' accessor is only valid when experimental feature coroutine accessors is enabled}}
    yield &_i
  }
}

// enabled: ok
// disabled: implicit getter.
var im_r: Int {
  modify { // expected-disabled-error{{cannot_find_in_scope}}
    fatalError()
  }
  _read { // expected-disabled-error{{cannot_find_in_scope}}
    fatalError()
  }
}

// enabled: ok
// disabled: implicit getter
var irm: Int {
  read { // expected-disabled-error{{cannot_find_in_scope}}
    fatalError()
  }
  modify { // expected-disabled-error{{cannot_find_in_scope}}
    fatalError()
  }
}

// enabled: ok
// disabled: implicit getter.
var imr: Int {
  modify { // expected-disabled-error{{cannot_find_in_scope}}
    fatalError()
  }
  read { // expected-disabled-error{{cannot_find_in_scope}}
    fatalError()
  }
}

// enabled: ok
// disabled: implicit getter
var irs: Int {
  read { // expected-disabled-error{{cannot_find_in_scope}}
    fatalError()
  }
  set { // expected-disabled-error{{cannot_find_in_scope}}
  }
}

// enabled: ok
// disabled: bad keyword
var isr: Int {
  set {
  }
  read { // expected-disabled-error{{'read' accessor is only valid when experimental feature coroutine accessors is enabled}}
    fatalError()
  }
}

// enabled: ok
// disabled: implicit getter.
var iruma: Int {
  read { // expected-disabled-error{{cannot_find_in_scope}}
    fatalError()
  }
  unsafeMutableAddress { // expected-disabled-error{{cannot_find_in_scope}}
    UnsafeMutablePointer<Int>(bitPattern: 0x0)!
  }
}

// enabled: ok
// disabled: implicit getter.
var iumar: Int {
  read { // expected-disabled-error{{cannot_find_in_scope}}
    fatalError()
  }
  unsafeMutableAddress { // expected-disabled-error{{cannot_find_in_scope}}
    UnsafeMutablePointer<Int>(bitPattern: 0x0)!
  }
}

// enabled: ok
// disabled: implicit getter
var ir_m: Int {
  read { // expected-disabled-error{{cannot_find_in_scope}}
    fatalError()
  }
  _modify { // expected-disabled-error{{cannot_find_in_scope}}
    fatalError()
  }
}

// enabled: ok
// disabled: bad keyword
var i_mr: Int {
  _modify {
    fatalError()
  }
  read { // expected-disabled-error{{'read' accessor is only valid when experimental feature coroutine accessors is enabled}}
    fatalError()
  }
}

// =============================================================================
// Multiple mutating only.
// =============================================================================

// enabled: need a reader.
// disabled: implicit getter.
var ims: Int {
  modify { // expected-disabled-error{{cannot_find_in_scope}}
    fatalError()
  }
  set { // expected-enabled-error{{variable with a setter must also have a getter, addressor, or 'read' accessor}}
        // expected-disabled-error@-1{{cannot_find_in_scope}}
  }
}

// enabled: need a reader
// disabled: bad keyword
var ism: Int {
  set { // expected-error{{variable with a setter must also have a getter, addressor, or 'read' accessor}}
    fatalError()
  }
  modify {// expected-disabled-error{{'modify' accessor is only valid when experimental feature coroutine accessors is enabled}}
    fatalError()
  }
}

// enabled: need a reader.
// disabled: implicit getter.
var imuma: Int {
  modify { // expected-enabled-error{{variable with a 'modify' accessor must also have a getter, addressor, or 'read' accessor}}
           // expected-disabled-error@-1{{cannot_find_in_scope}}
    fatalError()
  }
  unsafeMutableAddress { // expected-disabled-error{{cannot_find_in_scope}}
    UnsafeMutablePointer<Int>(bitPattern: 0x0)!
  }
}

// enabled: need a reader
// disabled: bad keyword
var iumam: Int {
  unsafeMutableAddress {
    fatalError()
  }
  modify { // expected-error{{variable with a 'modify' accessor must also have a getter, addressor, or 'read' accessor}}
           // expected-disabled-error@-1{{'modify' accessor is only valid when experimental feature coroutine accessors is enabled}}
    fatalError()
  }
}

// enabled: conflicting accessors.  need a reader.
// disabled: implicit getter.
var im_m: Int {
  modify { // expected-disabled-error{{cannot_find_in_scope}}
           // expected-enabled-error@-1{{variable cannot provide both a 'modify' accessor and a '_modify' accessor}}
    fatalError()
  }
  _modify { // expected-enabled-error{{variable with a '_modify' accessor must also have a getter, addressor, or 'read' accessor}}
            // expected-disabled-error@-1{{cannot_find_in_scope}}
            // expected-enabled-note@-2{{previous_accessor}}
    fatalError()
  }
}

// enabled: need a reader.
// disabled: implicit getter.
var i_mm: Int {
  _modify { // expected-enabled-error{{variable with a '_modify' accessor must also have a getter, addressor, or 'read' accessor}}
            // expected-disabled-error@-1{{variable with a 'modify' accessor must also have a getter, addressor, or 'read' accessor}}
            // expected-note@-2{{previous_accessor}}
    fatalError()
  }
  modify { // expected-disabled-error{{'modify' accessor is only valid when experimental feature coroutine accessors is enabled}}
           // expected-error@-1{{variable cannot provide both a 'modify' accessor and a '_modify' accessor}}
    fatalError()
  }
}

// enabled: need a reader.
// disabled: implicit getter.
var imm: Int {
  modify { // expected-enabled-error{{variable with a 'modify' accessor must also have a getter, addressor, or 'read' accessor}}
           // expected-disabled-error@-1{{cannot_find_in_scope}}
    fatalError()
  }
  modify { // expected-enabled-error{{variable already has a 'modify' accessor}}
           // expected-enabled-note@-5{{previous_accessor}}
           // expected-disabled-error@-2{{cannot_find_in_scope}}
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
  modify { // expected-disabled-error{{'modify' accessor is only valid when experimental feature coroutine accessors is enabled}}
           // expected-error@-1{{variable cannot provide both a 'modify' accessor and a '_modify' accessor}}
    yield &_i
  }
  _modify { // expected-note{{previous_accessor}}
    yield &_i
  }
}

// enabled: ok
// disabled: implicit getter
var ir_rm_m: Int {
  read { // expected-disabled-error{{cannot_find_in_scope}}
         // expected-enabled-error@-1{{variable cannot provide both a 'read' accessor and a '_read' accessor}}
    fatalError()
  }
  _read { // expected-disabled-error{{cannot_find_in_scope}}
          // expected-enabled-note@-1{{previous_accessor}}
    fatalError()
  }
  modify { // expected-disabled-error{{cannot_find_in_scope}}
           // expected-enabled-error@-1{{variable cannot provide both a 'modify' accessor and a '_modify' accessor}}
    fatalError()
  }
  _modify { // expected-disabled-error{{cannot_find_in_scope}}
            // expected-enabled-note@-1{{previous_accessor}}
    fatalError()
  }
}

// =============================================================================
// Protocol Requirements
// =============================================================================

protocol P {
  var goodP: Int { read set } //expected-disabled-error{{property in protocol must have explicit { get } or { get set } specifier}}
                              //expected-disabled-error@-1{{expected get or set in a protocol property}}
  var badP: Int { read modify } //expected-enabled-error{{expected get, read, or set in a protocol property}}
                                //expected-disabled-error@-1{{property in protocol must have explicit { get } or { get set } specifier}}
                                //expected-disabled-error@-2{{expected get or set in a protocol property}}
  subscript(goodS goodS: Int) -> Int { read set } //expected-disabled-error{{expected get or set in a protocol property}}
  subscript(badS badS: Int) -> Int { read modify } //expected-enabled-error{{expected get, read, or set in a protocol property}}
                                                   //expected-disabled-error@-1{{expected get or set in a protocol property}}
}
