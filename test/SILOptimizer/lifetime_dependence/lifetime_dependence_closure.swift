// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Lifetimes

struct NCInt: ~Copyable {
  var value: Int

  init(_ value: Int) { self.value = value }
}

struct NEInt : ~Escapable {
  let value: Int

  @_lifetime(borrow borrowed)
  init(borrowed: borrowing NCInt) {
    self.value = borrowed.value
  }
}

extension NCInt {
  var neInt: NEInt {
    borrowing get {
      NEInt(borrowed: self)
    }
  }
}

func ncint_get_neint_mutable_local() {
  var ncInt = NCInt(731)
  do {
    // Begin read access ncInt
    var neInt = ncInt.neInt

    // Capture neInt in a nonescapable closure.
    assert(ncInt.value == neInt.value)
    // End read access ncInt

    // Begin read access ncInt
    // Fully reassign neInt
    neInt = ncInt.neInt
    // Destroy neInt
    // End read access ncInt
  }
  ncInt.value = 1
}

struct NE: ~Escapable {
  public func condition() -> Bool {
    return true
  }
}

func takePicker(picker: @_lifetime(copy ne0, copy ne1) (_ ne0: NE, _ ne1: NE) -> NE) {
    let x = NE()
    let y = NE()
    _ = picker(x, y)
}

func takeOnePicker(picker: @_lifetime(copy ne0) (_ ne0: NE, NE) -> NE) {
    let x = NE()
    let y = NE()
    _ = picker(x, y)
}

func takeMutator(mutator: @_lifetime(ne: copy ne) (_ ne: inout NE) -> ()) {
  var ne = NE()
  mutator(&ne)
  let _ = ne
}

func testClosureLifetimes(cond: Bool) {
  takePicker { ne0, ne1 in ne0 } // OK
  takePicker { ne0, ne1 in ne1 } // OK

  takePicker { ne0, ne1 in if cond { return ne0 } else { return ne1 } } // OK

  // Several of the following error cases should start passing with no source
  // changes once closure context lifetimes are implemented. Some may require
  // explicit lifetime annotations; for those, add an extra test case with the
  // annotation.
  
  // OK, ne2 is captured but its lifetime is not related
  let ne2 = NE()
  takePicker { ne0, ne1 in
    if ne2.condition() { return ne0 } else { return ne1 }
  }

  let ne3 = NE()
  // expected-error@-1{{lifetime-dependent variable 'ne3' escapes its scope}}
  // expected-note@-2{{it depends on a closure capture; this is not yet supported}}
  takePicker { ne0, ne1 in ne3 }
  // expected-note@-1{{this use causes the lifetime-dependent value to escape}}

  var ne4 = NE() // expected-error{{lifetime-dependent variable 'ne4' escapes its scope}}
  takePicker { ne0, ne1 in
    ne4 = NE()   // expected-note{{it depends on the lifetime of this parent value}}
    return ne0   // expected-note{{this use causes the lifetime-dependent value to escape}}
  }
  let _ = ne4

  let ne5 = NE() // expected-note{{it depends on a closure capture; this is not yet supported}}
  takePicker { ne0, ne1 in
    let ne = ne5 // expected-error{{lifetime-dependent variable 'ne' escapes its scope}}
    return ne    // expected-note{{this use causes the lifetime-dependent value to escape}}
  }

  var ne6 = NE()           // expected-error{{lifetime-dependent variable 'ne6' escapes its scope}}
  takePicker { ne0, ne1 in // expected-note{{it depends on the lifetime of argument 'ne1'}}
    ne6 = ne1
    return ne0             // expected-note{{this use causes the lifetime-dependent value to escape}}
  }
  let _ = ne6

  takeMutator { ne0 in // OK
    let neLocal = ne0
    ne0 = neLocal
  }

  let ne8 = NE()       // expected-note{{it depends on a closure capture; this is not yet supported}}
  takeMutator { ne0 in // expected-error{{lifetime-dependent variable 'ne0' escapes its scope}}
    ne0 = ne8
  }                    // expected-note{{this use causes the lifetime-dependent value to escape}}


  takeOnePicker { ne0, ne1 in ne0 } // OK
  takeOnePicker { ne0, ne1 in ne1 }
  // expected-error@-1{{lifetime-dependent variable 'ne1' escapes its scope}}
  // expected-note@-2{{it depends on the lifetime of argument 'ne1'}}
  // expected-note@-3{{this use causes the lifetime-dependent value to escape}}

  takeOnePicker { ne0, ne1 in
                 // expected-error@-1{{lifetime-dependent variable 'ne1' escapes its scope}}
                 // expected-note@-2{{it depends on the lifetime of argument 'ne1'}}
    if cond {
      return ne0
    } else {
      return ne1 // expected-note{{this use causes the lifetime-dependent value to escape}}
    }
  }
}

// MARK: Methods with lifetime dependencies
func transfer(ne: NE) -> NE { ne }
struct S {
  static func staticTransfer(ne: NE) -> NE { ne }
  func transfer(ne: NE) -> NE { ne }
  @_lifetime(borrow self, copy ne)
  static func staticTransferDependingOnSelf(ne: NE) -> NE { ne }
  @_lifetime(borrow self, copy ne)
  func transferDependingOnSelf(ne: NE) -> NE { ne }
}

do {
  let _ = transfer // OK
  let _: (NE) -> NE = transfer // OK
  let _: @_lifetime(copy ne) (_ ne: NE) -> NE = transfer // OK
  let _: @_lifetime(copy ne) (_ ne: NE) -> NE = S.staticTransfer // OK, method is static
  let _: (_ ne: NE) -> NE = S.staticTransferDependingOnSelf
  // expected-error@-1{{lifetime-dependent value escapes its scope}}
  // expected-note@-2{{it depends on the lifetime of argument 'ne'}}
  // expected-note@-3{{this use causes the lifetime-dependent value to escape}}
  // expected-error@-4{{lifetime-dependent value escapes its scope}}
  // expected-note@-5{{it depends on the lifetime of this parent value}}
  // expected-note@-6{{this use causes the lifetime-dependent value to escape}}

  // Non-static methods are not supported yet
  let s = S()
  let _: @_lifetime(copy ne) (_ ne: NE) -> NE = s.transfer
  // expected-error@-1{{lifetime-dependent value escapes its scope}}
  // expected-note@-2{{it depends on the lifetime of argument 'ne'}}
  // expected-note@-3{{this use causes the lifetime-dependent value to escape}}
  let _: @_lifetime(copy ne) (_ ne: NE) -> NE = s.transferDependingOnSelf
  // expected-error@-1{{lifetime-dependent value escapes its scope}}
  // expected-note@-2{{it depends on the lifetime of argument 'ne'}}
  // expected-note@-3{{this use causes the lifetime-dependent value to escape}}
  // expected-error@-4{{lifetime-dependent value escapes its scope}}
  // expected-note@-5{{it depends on a closure capture; this is not yet supported}}
  // expected-note@-6{{this use causes the lifetime-dependent value to escape}}
}
