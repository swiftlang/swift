// RUN: %target-typecheck-verify-swift -D FOO -D BAZ -swift-version 4

// <https://twitter.com/practicalswift/status/829066902869786625>
#if // expected-error {{incomplete condition in conditional compilation directive}}
#if 0 == // expected-cparser-error {{incomplete condition in conditional compilation directive}}
#if 0= // expected-cparser-error {{incomplete condition in conditional compilation directive}} expected-cparser-error {{'=' must have consistent whitespace on both sides}}
class Foo {
  #if // expected-error {{incomplete condition in conditional compilation directive}}
  #if 0 == // expected-cparser-error {{incomplete condition in conditional compilation directive}}
  #if 0= // expected-cparser-error {{incomplete condition in conditional compilation directive}} expected-cparser-error {{'=' must have consistent whitespace on both sides}}
}
