// RUN: %target-parse-verify-swift

struct S {
  var a : Int ;
  func b () {};
  static func c () {};
}

struct SpuriousSemi {
  ; // expected-error{{unexpected ';' separator}} {{3-5=}}
  var a : Int ; ; // FIXME -- we need to consistently track ','/';' separators
  func b () {};
  ; static func c () {} // FIXME -- we need to consistently track ','/';' separators
}

class C {
  var a : Int = 10;
  func b () {};
  class func c () {};
}

extension S {
  //var a : Int ;
  func bb () {};
  static func cc () {};
}

protocol P {
  var a : Int { get };
  func b ();
  static func c ();
}

