// RUN: %target-swift-frontend -primary-file %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test

// REQUIRES: swift_in_compiler

// REQUIRES: rdar132874319
// ([nonescapable] improve diagnostics for spanofspans.swift; prefer an exclusivity violation to a lifetime error)

// TODO: uncomment the @lifetime annotations when we have component lifetimes.

// @lifetime(copy elements)
struct Span<T: ~Escapable>: ~Escapable {
  // Pretend that 'element' is in separate storage.
  var element: T

  // @lifetime(elements: t)
  init(t: T) {
    self.element = t
  }
}

// @lifetime(copy elements)
extension Array {
  // @lifetime(span: borrow self)
  // @lifetime(span.elements: copy self.elements)
  var span: Span<Element> {
    _read {
      yield Span(t: first!)
    }
  }
}

// use 'scalars' instead of 'elements' to avoid confusion from nesting
// @lifetime(copy scalars)
struct Vec<T: ~Escapable>: ~Escapable {
  // Pretend that 't' is in separate storage.
  var scalar: T

  // @lifetime(scalars: t)
  init(t: T) {
    self.scalar = t
  }

  // @lifetime(span: self)
  // @lifetime(span.elements: self.scalars)
  public var span: Span<T> { // expected-note {{it depends on the lifetime of argument 'self'}}
    _read {
      yield Span(t: scalar)
    }
  }
}

func useSpan<T>(_: Span<T>) {}

func withFatlifetimes() {
  let arr = [1,2,3] // expected-note {{it depends on the lifetime of variable 'arr'}}
  // span depends on arr
  let span = arr.span
  // vec depends on arr
  var vec = Vec(t: span) // expected-error {{lifetime-dependent value escapes its scope}}
                         // expected-note @-3{{it depends on the lifetime of variable 'arr'}}
                         // expected-note @-1{{this use causes the lifetime-dependent value to escape}}
  // spanOfSpans depends on borrow vec
  let spanOfSpans = vec.span
  // firstSpan depends on spanOfSpans
  let firstSpan = spanOfSpans.element
  // 🛑 ERROR: vec mutation during borrow
  vec.scalar = arr.span // expected-error {{lifetime-dependent value escapes its scope}}
                        // expected-note {{this use causes the lifetime-dependent value to escape}}
  // (firstSpan depends on vec)
  useSpan(firstSpan)
  // end borrow vec
}
