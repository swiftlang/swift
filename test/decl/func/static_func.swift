// RUN: %target-typecheck-verify-swift

static func gf1() {} // expected-error {{static methods may only be declared on a type}}{{1-8=}}
class func gf2() {} // expected-error {{class methods may only be declared on a type}}{{1-7=}}

override static func gf3() {} // expected-error {{static methods may only be declared on a type}}{{10-17=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{1-10=}}
override class func gf4() {} // expected-error {{class methods may only be declared on a type}}{{10-16=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{1-10=}}

static override func gf5() {} // expected-error {{static methods may only be declared on a type}}{{1-8=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{8-17=}}
class override func gf6() {} // expected-error {{class methods may only be declared on a type}}{{1-7=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{7-16=}}

static gf7() {} // expected-error {{expected declaration}} expected-error {{closure expression is unused}} expected-error{{begin with a closure}} expected-note{{did you mean to use a 'do' statement?}} {{14-14=do }}
class gf8() {} // expected-error {{expected '{' in class}} expected-error {{closure expression is unused}} expected-error{{begin with a closure}} expected-note{{did you mean to use a 'do' statement?}} {{13-13=do }}

func inGlobalFunc() {
  static func gf1() {} // expected-error {{static methods may only be declared on a type}}{{3-10=}}
  class func gf2() {} // expected-error {{class methods may only be declared on a type}}{{3-9=}}
}

struct InMemberFunc {
  func member() {
    static func gf1() {} // expected-error {{static methods may only be declared on a type}}{{5-12=}}
    class func gf2() {} // expected-error {{class methods may only be declared on a type}}{{5-11=}}
  }
}

struct DuplicateStatic {
  static static func f1() {} // expected-error{{'static' specified twice}}{{10-17=}}
  static class func f2() {} // expected-error{{'class' specified twice}}{{10-16=}}
  class static func f3() {} // expected-error{{'static' specified twice}}{{9-16=}} expected-error{{class methods are only allowed within classes; use 'static' to declare a static method}}{{3-8=static}}
  class class func f4() {} // expected-error{{'class' specified twice}}{{9-15=}} expected-error{{class methods are only allowed within classes; use 'static' to declare a static method}}{{3-8=static}}
  override static static func f5() {} // expected-error{{'static' specified twice}}{{19-26=}} expected-error{{'override' can only be specified on class members}} {{3-12=}}
  static override static func f6() {} // expected-error{{'static' specified twice}}{{19-26=}} expected-error{{'override' can only be specified on class members}} {{10-19=}}
  static static override func f7() {} // expected-error{{'static' specified twice}}{{10-17=}} expected-error{{'override' can only be specified on class members}} {{17-26=}}
  static final func f8() {} // expected-error {{only classes and class members may be marked with 'final'}}
}

struct S { // expected-note {{extended type declared here}}
  static func f1() {}
  class func f2() {} // expected-error {{class methods are only allowed within classes; use 'static' to declare a static method}} {{3-8=static}}
}

extension S {
  static func ef1() {}
  class func ef2() {} // expected-error {{class methods are only allowed within classes; use 'static' to declare a static method}} {{3-8=static}}
}

enum E { // expected-note {{extended type declared here}}
  static func f1() {}
  class func f2() {} // expected-error {{class methods are only allowed within classes; use 'static' to declare a static method}} {{3-8=static}}
  static final func f3() {} // expected-error {{only classes and class members may be marked with 'final'}}
}

extension E {
  static func f4() {}
  class func f5() {} // expected-error {{class methods are only allowed within classes; use 'static' to declare a static method}} {{3-8=static}}
}

class C {
  static func f1() {} // expected-note 3{{overridden declaration is here}}
  class func f2() {}
  class func f3() {}
  class func f4() {} // expected-note {{overridden declaration is here}}
  class func f5() {} // expected-note {{overridden declaration is here}}
  static final func f6() {} // expected-error {{static declarations are already final}} {{10-16=}}
  final class func f7() {} // expected-note 3{{overridden declaration is here}}
}

extension C {
  static func ef1() {}
  class func ef2() {} // expected-note {{overridden declaration is here}}
  class func ef3() {} // expected-note {{overridden declaration is here}}
  class func ef4() {} // expected-note {{overridden declaration is here}}
  class func ef5() {} // expected-note {{overridden declaration is here}}
}

class C_Derived : C {
  override static func f1() {} // expected-error {{cannot override static method}}
  override class func f2() {}
  class override func f3() {}

  override class func ef2() {} // expected-error {{declarations from extensions cannot be overridden yet}}
  class override func ef3() {} // expected-error {{declarations from extensions cannot be overridden yet}}
  override static func f7() {} // expected-error {{static method overrides a 'final' class method}}
}

class C_Derived2 : C {
  override final class func f1() {} // expected-error {{cannot override static method}}
  override final class func f7() {} // expected-error {{class method overrides a 'final' class method}}
}
class C_Derived3 : C {
  override class func f1() {} // expected-error {{cannot override static method}}
  override class func f7() {} // expected-error {{class method overrides a 'final' class method}}
}

extension C_Derived {
  override class func f4() {} // expected-error {{declarations in extensions cannot override yet}}
  class override func f5() {} // expected-error {{declarations in extensions cannot override yet}}

  override class func ef4() {} // expected-error {{declarations in extensions cannot override yet}}
  class override func ef5() {} // expected-error {{declarations in extensions cannot override yet}}
}

protocol P {
  static func f1()
  static func f2()
  static func f3() {} // expected-error {{protocol methods may not have bodies}}
  static final func f4() // expected-error {{only classes and class members may be marked with 'final'}}
}

