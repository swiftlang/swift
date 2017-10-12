// RUN: %target-swift-frontend -emit-sil -enable-sil-ownership %s -o /dev/null -verify

struct EmptyStruct {}

struct ValueStruct {
  var ivar: EmptyStruct // expected-note {{'self.ivar' not initialized}}

  init() { ivar = EmptyStruct() }


  init(a: Double) {
    self.init()
    _ = ivar // okay: ivar has been initialized by the delegation above
  }

  init(a: Int) {
    _ = ivar // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
    self.init()
  }

  init(a: Float) {
    self.init()
    self.init() // expected-error {{'self.init' called multiple times in initializer}}
  }

  init(c: Bool) {
    if c {
      return
    }

    self.init()
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}

  init(d: Bool) {
    if d {
      return // expected-error {{return from initializer without initializing all stored properties}}
    }

    self = ValueStruct()
  }

  // FIXME: doesn't work yet
  init(e: Bool) {
    if e {
      self.init()
    } else {
      self = ValueStruct() // expected-error {{}}
    }
  } // expected-error {{}}
}

enum ValueEnum {
  case Dinosaur, Train, Truck

  init() { self = .Train }

  init(a: Double) {
    self.init()
    _ = self // okay: self has been initialized by the delegation above
    self = .Dinosaur
  }

  init(a: Int) {
    _ = self // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
    self.init()
  }

  init(a: Float) {
    self.init()
    self.init() // expected-error {{'self.init' called multiple times in initializer}}
  }

  init(c: Bool) {
    if c {
      return
    }

    self.init()
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}

  init(d: Bool) {
    if d {
      return // expected-error {{return from initializer before 'self.init' call or assignment to 'self'}}
    }

    self = ValueEnum()
  }

  // FIXME: doesn't work yet
  init(e: Bool) {
    if e {
      self = ValueEnum() // expected-error {{}}
    } else {
      self.init()
    }
  } // expected-error {{}}
}

struct AddressStruct {
  var ivar: EmptyStruct // expected-note {{'self.ivar' not initialized}}
  var any: Any?

  init() { ivar = EmptyStruct(); any = nil }

  init(c: Bool) {
    if c {
      return
    }

    self.init()
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}

  init(d: Bool) {
    if d {
      return
    }

    self = AddressStruct()
  } // expected-error {{return from initializer without initializing all stored properties}}

  // FIXME: doesn't work yet
  init(e: Bool) {
    if e {
      self = AddressStruct() // expected-error {{}}
    } else {
      self.init()
    }
  } // expected-error {{}}
}
