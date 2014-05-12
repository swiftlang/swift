// RUN: %swift -parse %s -verify

static func gf1() {} // expected-error {{static functions may only be declared on a type}}{{1-7=}}
class func gf2() {} // expected-error {{class methods may only be declared on a type}}{{1-6=}}

override static func gf3() {} // expected-error {{static functions may only be declared on a type}}{{10-16=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{1-9=}}
override class func gf4() {} // expected-error {{class methods may only be declared on a type}}{{10-15=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{1-9=}}

static override func gf5() {} // expected-error {{static functions may only be declared on a type}}{{1-7=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{8-16=}}
class override func gf6() {} // expected-error {{class methods may only be declared on a type}}{{1-6=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{7-15=}}

static gf7() {} // expected-error {{expected declaration}} expected-error {{braced block of statements is an unused closure}}
class gf8() {} // expected-error {{expected '{' in class}} expected-error {{braced block of statements is an unused closure}}

func inGlobalFunc() {
  static func gf1() {} // expected-error {{static functions may only be declared on a type}}{{3-9=}}
  class func gf2() {} // expected-error {{class methods may only be declared on a type}}{{3-8=}}
}

struct InMemberFunc {
  func member() {
    static func gf1() {} // expected-error {{static functions may only be declared on a type}}{{5-11=}}
    class func gf2() {} // expected-error {{class methods may only be declared on a type}}{{5-10=}}
  }
}

struct DuplicateStatic {
  static static func f1() {} // expected-error{{'static' specified twice}}{{10-16=}}
  static class func f2() {} // expected-error{{'class' specified twice}}{{10-15=}}
  class static func f3() {} // expected-error{{'static' specified twice}}{{9-15=}} expected-error{{class methods are only allowed within classes and protocols; use 'static' to declare a static function}}{{3-8=static}}
  class class func f4() {} // expected-error{{'class' specified twice}}{{9-14=}} expected-error{{class methods are only allowed within classes and protocols; use 'static' to declare a static function}}{{3-8=static}}
  override static static func f5() {} // expected-error{{'static' specified twice}}{{19-25=}} expected-error{{'override' can only be specified on class members}}
  static override static func f6() {} // expected-error{{'static' specified twice}}{{19-25=}} expected-error{{'override' can only be specified on class members}}
  static static override func f7() {} // expected-error{{'static' specified twice}}{{10-16=}} expected-error{{'override' can only be specified on class members}}
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
  static func f3() {}
  class func f4() {} // expected-error {{class methods are only allowed within classes and protocols; use 'static' to declare a static function}}{{3-8=static}}
}

class C { // expected-note {{extended type declared here}}
  static func f1() {} // expected-error {{static functions are only allowed within structs and enums; use 'class' to declare a class method}}{{3-9=class}}
  class func f2() {}
  class func f3() {}
  class func f4() {} // expected-note {{overridden declaration is here}}
  class func f5() {} // expected-note {{overridden declaration is here}}
}

extension C {
  static func ef1() {} // expected-error {{static functions are only allowed within structs and enums; use 'class' to declare a class method}}{{3-9=class}}
  class func ef2() {} // expected-note {{overridden declaration is here}}
  class func ef3() {} // expected-note {{overridden declaration is here}}
  class func ef4() {} // expected-note {{overridden declaration is here}}
  class func ef5() {} // expected-note {{overridden declaration is here}}
}

class C_Derived : C {
  override class func f2() {}
  class override func f3() {}

  override class func ef2() {} // expected-error {{declarations from extensions cannot be overridden yet}}
  class override func ef3() {} // expected-error {{declarations from extensions cannot be overridden yet}}
}

extension C_Derived {
  override class func f4() {} // expected-error {{declarations in extensions cannot override yet}}
  class override func f5() {} // expected-error {{declarations in extensions cannot override yet}}

  override class func ef4() {} // expected-error {{declarations in extensions cannot override yet}}
  class override func ef5() {} // expected-error {{declarations in extensions cannot override yet}}
}

protocol P {
  static func f1() // expected-error {{static functions are only allowed within structs and enums; use 'class' to declare a class method}}{{3-9=class}}
  class func f2() {} // expected-error {{protocol methods may not have bodies}}
}

extension P { // expected-error {{protocol 'P' cannot be extended}}
}

