// RUN: %target-typecheck-verify-swift \
// RUN:     -verify-additional-prefix enabled- \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -debug-diagnostic-names
// RUN: %target-typecheck-verify-swift \
// RUN:     -verify-additional-prefix disabled- \
// RUN:     -debug-diagnostic-names

var _i: Int = 0

// Order of accessor kinds:
// readers:
// - get
// - unsafeAddress
// - _read
// writers:
// - set
// - unsafeMutableAddress
// - _modify
// - modify

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

var im_r: Int {
  modify { // expected-disabled-error{{cannot_find_in_scope}}
    fatalError()
  }
  _read { // expected-disabled-error{{cannot_find_in_scope}}
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

// enabled: need a reader.
// disabled: implicit getter.
var im_m: Int {
  modify { // expected-disabled-error{{cannot_find_in_scope}}
    fatalError()
  }
  _modify { // expected-enabled-error{{variable with a 'modify' accessor must also have a getter, addressor, or 'read' accessor}}
             // expected-disabled-error@-1{{cannot_find_in_scope}}
    fatalError()
  }
}

// enabled: need a reader.
// disabled: implicit getter.
var i_mm: Int {
  _modify { // expected-error{{variable with a 'modify' accessor must also have a getter, addressor, or 'read' accessor}}
    fatalError()
  }
  modify { // expected-disabled-error{{'modify' accessor is only valid when experimental feature coroutine accessors is enabled}}
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
    yield &_i
  }
  _modify {
    yield &_i
  }
}
