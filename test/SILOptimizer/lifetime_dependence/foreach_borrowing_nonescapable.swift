// RUN: %target-swift-frontend %s -emit-sil -verify \
// RUN:     -enable-experimental-feature BorrowingForLoop \
// RUN:     -enable-experimental-feature BorrowingSequence \
// RUN:     -enable-experimental-feature Lifetimes \
// RUN:     -verify-ignore-unrelated

// REQUIRES: swift_feature_BorrowingForLoop
// REQUIRES: swift_feature_BorrowingSequence
// REQUIRES: swift_feature_Lifetimes

struct NE: ~Escapable {
  var value: Int
  @_lifetime(immortal)
  init(value: Int) { self.value = value }
}

@available(SwiftStdlib 6.4, *)
func testEscapeFromForLoop(span: borrowing Span<NE>) {
  var escaped: NE = NE(value: 0) // expected-error {{lifetime-dependent variable 'escaped' escapes its scope}}
  for el in span {  // expected-note {{it depends on this scoped access to variable '$el$generator'}}
      escaped = el
  }
  _ = escaped // expected-note {{this use of the lifetime-dependent value is out of scope}}
}

@available(SwiftStdlib 6.4, *)
func testEscapeNoUseOutsideForLoop(span: borrowing Span<NE>) {
  var escaped: NE = NE(value: 0)
  for el in span {
      escaped = el
      _ = escaped
  }
}

func takeEscaping(_ f: @escaping () -> NE) {}

@available(SwiftStdlib 6.4, *)
func testEscapeViaClosure(span: borrowing Span<NE>) { // expected-note 2 {{it depends on the lifetime of argument 'span'}}
  for el in span {  // expected-note {{it depends on this scoped access to variable '$el$generator'}}
    // expected-error@-1 3 {{lifetime-dependent variable 'el' escapes its scope}}
    takeEscaping { el } // expected-note 3 {{this use causes the lifetime-dependent value to escape}}
  }
}

@available(SwiftStdlib 6.4, *)
func testNestedLoopEscape(outer: borrowing Span<Int>, inner: borrowing Span<NE>) {
  var saved: NE = NE(value: 0) // expected-error {{lifetime-dependent variable 'saved' escapes its scope}}
  for _ in outer {
    for el in inner { // expected-note {{it depends on this scoped access to variable '$el$generator'}}
      saved = el
    }
    _ = saved // expected-note {{this use of the lifetime-dependent value is out of scope}}
  }
}

@available(SwiftStdlib 6.4, *)
@_lifetime(borrow span)
func testReturnFromLoop(span: borrowing Span<NE>) -> NE {
  for el in span { // expected-error {{lifetime-dependent variable 'el' escapes its scope}}
    // expected-note@-1 {{it depends on this scoped access to variable '$el$generator'}}
    return el // expected-note {{this use causes the lifetime-dependent value to escape}}
  }
  return NE(value: -1)
}

@available(SwiftStdlib 6.4, *)
func testEscapeWithWhereClause(span: borrowing Span<NE>) {
  var saved: NE = NE(value: 0) // expected-error {{lifetime-dependent variable 'saved' escapes its scope}}
  for el in span where el.value > 0 { // expected-note {{it depends on this scoped access to variable '$el$generator'}}
    saved = el
  }
  _ = saved // expected-note {{this use of the lifetime-dependent value is out of scope}}
}

@available(SwiftStdlib 6.4, *)
func testNoEscapeWithWhereClause(span: borrowing Span<NE>) {
  var saved: NE = NE(value: 0)
  for el in span where el.value > 0 {
    saved = el
    _ = saved
  }
}
