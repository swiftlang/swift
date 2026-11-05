// RUN: %target-typecheck-verify-swift

struct S {
  var a : Int ;
  func b () {};
  static func c () {};
}

struct SpuriousSemi {
  ; // expected-cparser-error{{unexpected ';' separator}} {{3-5=}} expected-astgen-error {{unexpected ';' separator}}
  var a : Int ; ; // expected-cparser-error{{unexpected ';' separator}} {{17-19=}} expected-astgen-error {{unexpected ';' separator}}
  func b () {};
  ; static func c () {};  // expected-cparser-error{{unexpected ';' separator}} {{3-5=}} expected-astgen-error {{unexpected ';' separator}}
  ;;
  // expected-cparser-error @-1 {{unexpected ';' separator}} {{3-4=}} expected-astgen-error@-1 {{unexpected ';' separator}}
  // expected-cparser-error @-2 {{unexpected ';' separator}} {{4-5=}}
}

class C {
  var a : Int = 10 func aa() {}; // expected-cparser-error {{consecutive declarations on a line must be separated by ';'}} {{19-19=;}} expected-astgen-error {{consecutive declarations on a line must be separated by newline or ';'}}
#if FLAG1
  var aaa: Int = 42 func aaaa() {}; // expected-cparser-error {{consecutive declarations on a line must be separated by ';'}} {{20-20=;}} expected-astgen-error {{consecutive declarations on a line must be separated by newline or ';'}}
#elseif FLAG2
  var aaa: Int = 42 func aaaa() {} // expected-cparser-error {{consecutive declarations on a line must be separated by ';'}} {{20-20=;}} expected-astgen-error {{consecutive declarations on a line must be separated by newline or ';'}}
#else
  var aaa: Int = 42 func aaaa() {} // expected-cparser-error {{consecutive declarations on a line must be separated by ';'}} {{20-20=;}} expected-astgen-error {{consecutive declarations on a line must be separated by newline or ';'}}
#endif

  func b () {};
  class func c () {};
}

extension S {
  //var a : Int ;
  func bb () {};
  static func cc () {};
  func dd() {} subscript(i: Int) -> Int { return 1 } // expected-cparser-error {{consecutive declarations on a line must be separated by ';'}} {{15-15=;}} expected-astgen-error {{consecutive declarations on a line must be separated by newline or ';'}}
}

protocol P {
  var a : Int { get };
  func b ();
  static func c ();
}
