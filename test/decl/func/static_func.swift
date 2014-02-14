// RUN: %swift -parse %s -verify

static func gf1() {} // expected-error {{static functions may only be declared on a type}}{{1-8=}}
class func gf2() {} // expected-error {{class methods may only be declared on a type}}{{1-7=}}

static gf3() {} // expected-error {{expected declaration}} expected-error {{braced block of statements is an unused closure}}
class gf4() {} // expected-error {{expected '{' in class}} expected-error {{braced block of statements is an unused closure}}

func inGlobalFunc() {
  static func gf1() {} // expected-error {{static functions may only be declared on a type}}{{3-10=}}
  class func gf2() {} // expected-error {{class methods may only be declared on a type}}{{3-9=}}
}

struct InMemberFunc {
  func member() {
    static func gf1() {} // expected-error {{static functions may only be declared on a type}}{{5-12=}}
    class func gf2() {} // expected-error {{class methods may only be declared on a type}}{{5-11=}}
  }
}

struct S { // expected-note {{extended type declared here}}
  static func f1() {}
  class func f2() {} // expected-error {{class methods are only allowed within classes and protocols; use 'static' to declare a static function}}{{3-8=static}}
}

extension S {
  static func ef1() {}
  class func ef2() {} // expected-error {{class methods are only allowed within classes and protocols; use 'static' to declare a static function}}{{3-8=static}}
}

enum E { // expected-note {{extended type declared here}}
  static func f1() {}
  class func f2() {} // expected-error {{class methods are only allowed within classes and protocols; use 'static' to declare a static function}}{{3-8=static}}
}

extension E {
  static func f1() {}
  class func f2() {} // expected-error {{class methods are only allowed within classes and protocols; use 'static' to declare a static function}}{{3-8=static}}
}

class C { // expected-note {{extended type declared here}}
  static func f1() {} // expected-error {{static functions are only allowed within structs and enums; use 'class' to declare a class method}}{{3-9=class}}
  class func f2() {}
}

extension C {
  static func ef1() {} // expected-error {{static functions are only allowed within structs and enums; use 'class' to declare a class method}}{{3-9=class}}
  class func ef2() {}
}

protocol P {
  static func f1() {} // expected-error {{static functions are only allowed within structs and enums; use 'class' to declare a class method}}{{3-9=class}}
  class func f2() {}
}

extension P { // expected-error {{protocol 'P' cannot be extended}}
}

