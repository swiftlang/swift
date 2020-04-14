// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s

struct Foo { }

//===--- Test that we don't crash when validating members inside an extension with no type name.
extension { // expected-error {{expected type name in extension declaration}}
  static func ==(lhs: Foo, rhs: Foo) -> Bool {} // expected-error {{operator '==' declared in extension must be 'static'}}
}
