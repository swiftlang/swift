// expected-error {{file name is not a valid identifier}}
// FIXME: this should be recognized and whitelisted as a script file

// RUN: %swift %s -verify

struct S {
  var a : Int ;
  func b ();
  static func c ();
}

struct SpuriousSemi {
  ; // expected-error{{semicolon is not allowed here}}
  var a : Int ; ; // expected-error{{semicolon is not allowed here}}
  func b ();
  ; static func c () // expected-error{{semicolon is not allowed here}}
}

class C {
  var a : Int ;
  func b ();
  static func c ();
}

extension S {
  //var a : Int ;
  func b ();
  static func c ();
}

protocol P {
  var a : Int ;
  func b ();
  static func c ();
}

