// RUN: %swift-frontend -typecheck %s
// RUN: %swift-frontend -typecheck %s -verify -DHAVE_NCGENERICS



/// This test checks that you can write ~Copyable in places that were illegal
/// in Swift 5.9, as long as those illegal appearances are guarded within
/// a disabled #if block.
///
/// We previously would emit errors during Parse for those illegal appearances,
/// which would make it impossible to feature-guard new, legal uses of those
/// annotations. Now we delay those diagnostics until Sema.
///
/// So, the first RUN line here checks that there are no error diagnostics emitted
/// when not providing HAVE_NCGENERICS. Then, we enable that experimental feature to
/// ensure that what's in the #if has no unexpected errors. Finally, we run without that
/// feature to ensure we get the expected errors when the #if-block is allowed to be
/// typechecked.

struct NC: ~Copyable {}

#if HAVE_NCGENERICS

struct Blah: ~Copyable, ~Copyable {}
// expected-error@-1 {{duplicate suppression of 'Copyable'}}

protocol Compare where Self: ~Copyable { // expected-error {{cannot suppress conformances here}}
                                         // expected-note@-1 {{type 'NC' does not conform to inherited protocol 'Copyable'}}
  func lessThan(_ other: borrowing Self) -> Bool
}

protocol Sortable<Element>: ~Copyable { // expected-error {{cannot suppress conformances here}}
                                        // expected-note@-1 {{type 'NC' does not conform to inherited protocol 'Copyable'}}
  associatedtype Element: ~Copyable, Compare // expected-error {{cannot suppress conformances here}}

  mutating func sort()
}

extension NC: Compare { // expected-error {{type 'NC' does not conform to protocol 'Copyable'}}
  func lessThan(_ other: borrowing Self) -> Bool { false }
}

extension NC: Sortable { // expected-error {{type 'NC' does not conform to protocol 'Copyable'}}
  typealias Element = NC

  mutating func sort() { }
}

func f<T>(_ t: inout T) // expected-error {{no type for 'T.Element' can satisfy both 'T.Element == NC' and 'T.Element : Copyable'}}
  where T: ~Copyable,  // expected-error {{cannot suppress conformances here}}
        T: Sortable,
        T.Element == NC {
  t.sort()
}

do {
  var x = NC()
  f(&x)
}

#endif
