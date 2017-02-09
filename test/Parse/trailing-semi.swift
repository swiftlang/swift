// RUN: %target-typecheck-verify-swift

struct S {
  var a : Int ;
  func b () {};
  static func c () {};
}

struct SpuriousSemi {
  ; // expected-error{{unexpected ';' separator}} {{3-5=}}
  var a : Int ; ; // expected-error{{unexpected ';' separator}} {{17-19=}}
  func b () {};
  ; static func c () {};  // expected-error{{unexpected ';' separator}} {{3-5=}}
  ;;
  // expected-error @-1 {{unexpected ';' separator}} {{3-4=}}
  // expected-error @-2 {{unexpected ';' separator}} {{4-5=}}
}

class C {
  var a : Int = 10 func aa() {}; // expected-error {{consecutive declarations on a line must be separated by ';'}} {{19-19=;}}
#if FLAG1
  var aaa: Int = 42 func aaaa() {}; // expected-error {{consecutive declarations on a line must be separated by ';'}} {{20-20=;}}
#elseif FLAG2
  var aaa: Int = 42 func aaaa() {} // expected-error {{consecutive declarations on a line must be separated by ';'}} {{20-20=;}}
#else
  var aaa: Int = 42 func aaaa() {} // expected-error {{consecutive declarations on a line must be separated by ';'}} {{20-20=;}}
#endif

  func b () {};
  class func c () {};
}

extension S {
  //var a : Int ;
  func bb () {};
  static func cc () {};
  func dd() {} subscript(i: Int) -> Int { return 1 } // expected-error {{consecutive declarations on a line must be separated by ';'}} {{15-15=;}}
}

protocol P {
  var a : Int { get };
  func b ();
  static func c ();
}
