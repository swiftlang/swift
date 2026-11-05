// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -target %target-swift-6.2-abi-triple \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_Lifetimes

// rdar://190361126 (GH #93414) Lifetime dependence is lost when an address-only ~Escapable value is stored into an
// Optional.
//
// An address-only enum is built in place: init_enum_data_addr + store of the payload + inject_enum_addr. The
// inject_enum_addr must not be treated as a reassignment that kills the payload's dependence.

struct View: ~Escapable {
  let span: Span<UInt8>
  let tag: Any = () // address-only

  @_lifetime(copy span)
  init(_ span: Span<UInt8>) { self.span = span }
}

struct Owner {
  var bytes: [UInt8] = [1, 2, 3, 4]

  var view: View {
    @_lifetime(borrow self)
    get { View(bytes.span) }
  }
}

enum Wrapper: ~Escapable {
  case some(View)
  case none
}

func withView(_ body: (borrowing View) -> ()) {}
func withOptionalView(_ body: (borrowing View?) -> ()) {}
func use(_ v: borrowing View?) {}
func use(_ w: borrowing Wrapper) {}

// Copy a closure parameter into a captured Optional.
func testClosureCapture() {
  var kept: View? = nil // expected-error {{lifetime-dependent variable 'kept' escapes its scope}}
  withView { v in kept = copy v }
  // expected-note @-1{{it depends on the lifetime of argument 'v'}}
  // expected-note @-2{{this use causes the lifetime-dependent value to escape}}
  use(kept)
}

// Unwrap an Optional closure parameter and rewrap it into a captured Optional.
func testClosureIfLet() {
  var kept: View? = nil // expected-error {{lifetime-dependent variable 'kept' escapes its scope}}
  withOptionalView { v in if let v = copy v { kept = v } }
  // expected-note @-1{{it depends on the lifetime of argument 'v'}}
  // expected-note @-2{{this use causes the lifetime-dependent value to escape}}
  use(kept)
}

// Copy a closure parameter into a captured non-Optional address-only enum.
func testClosureCustomEnum() {
  var kept: Wrapper = .none // expected-error {{lifetime-dependent variable 'kept' escapes its scope}}
  withView { v in kept = .some(copy v) }
  // expected-note @-1{{it depends on the lifetime of argument 'v'}}
  // expected-note @-2{{this use causes the lifetime-dependent value to escape}}
  use(kept)
}

// Assign a borrowed getter result to an outer var and use it after the owner goes out of scope.
func testAssignOutOfScope() {
  var kept: View? = nil // expected-error {{lifetime-dependent variable 'kept' escapes its scope}}
  do {
    let local = Owner() // expected-note {{it depends on the lifetime of variable 'local'}}
    kept = local.view
  }
  use(kept) // expected-note {{this use of the lifetime-dependent value is out of scope}}
}

// Initialize a deferred 'let' from a borrowed getter result and use it after the owner goes out of scope.
func testDeferredInitOutOfScope() {
  let kept: View? // expected-error {{lifetime-dependent variable 'kept' escapes its scope}}
  do {
    let local = Owner() // expected-note {{it depends on the lifetime of variable 'local'}}
    kept = local.view
  }
  use(kept) // expected-note {{this use of the lifetime-dependent value is out of scope}}
}

// Initialize an inner Optional in place, then copy it to an outer var.
func testInnerLocalCopiedOut() {
  var outer: View? = nil // expected-error {{lifetime-dependent variable 'outer' escapes its scope}}
  do {
    let local = Owner() // expected-note {{it depends on the lifetime of variable 'local'}}
    let inner: View? = local.view
    outer = inner
  }
  use(outer) // expected-note {{this use of the lifetime-dependent value is out of scope}}
}

// Store a borrowed getter result into an inout Optional that only copies its own lifetime.
@_lifetime(o: copy o)
func testInoutStore(_ o: inout View?, _ owner: borrowing Owner) {
  // expected-error @-1{{lifetime-dependent variable 'o' escapes its scope}}
  // expected-note @-2{{it depends on the lifetime of argument 'owner'}}
  o = owner.view
} // expected-note {{this use causes the lifetime-dependent value to escape}}

// Valid: the dependent Optional is used only within the owner's scope.
func testInScope() {
  let local = Owner()
  var kept: View? = nil
  kept = local.view
  use(kept)
}
