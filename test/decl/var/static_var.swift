// RUN: %swift -parse -parse-as-library %s -verify

// See also rdar://15626843.
static var gvu1: Int // expected-error {{static properties may only be declared on a type}}{{1-8=}}
    // expected-error@-1 {{global 'var' declaration require an initializer expression or getter/setter specifier}}
class var gvu2: Int // expected-error {{class properties may only be declared on a type}}{{1-7=}}
    // expected-error@-1 {{global 'var' declaration require an initializer expression or getter/setter specifier}}

static var gvu3: Int { // expected-error {{static properties may only be declared on a type}}{{1-8=}}
  return 42
}
class var gvu4: Int { // expected-error {{class properties may only be declared on a type}}{{1-7=}}
  return 42
}

static let glu1: Int // expected-error {{static properties may only be declared on a type}}{{1-8=}}
    // expected-error@-1 {{'let' declarations require an initializer expression}}
class let glu2: Int // expected-error {{class properties may only be declared on a type}}{{1-7=}}
    // expected-error@-1 {{'let' declarations require an initializer expression}}

static var gvi1: Int = 0 // expected-error {{static properties may only be declared on a type}}{{1-8=}}
class var gvi2: Int = 0 // expected-error {{class properties may only be declared on a type}}{{1-7=}}

static let gli1: Int = 0 // expected-error {{static properties may only be declared on a type}}{{1-8=}}
class let gli2: Int = 0 // expected-error {{class properties may only be declared on a type}}{{1-7=}}

func inGlobalFunc() {
  static var v1: Int // expected-error {{static properties may only be declared on a type}}{{3-10=}}
  class var v2: Int // expected-error {{class properties may only be declared on a type}}{{3-9=}}

  static let l1: Int = 0 // expected-error {{static properties may only be declared on a type}}{{3-10=}}
  class let l2: Int = 0 // expected-error {{class properties may only be declared on a type}}{{3-9=}}
}

struct InMemberFunc {
  func member() {
    static var v1: Int // expected-error {{static properties may only be declared on a type}}{{5-12=}}
    class var v2: Int // expected-error {{class properties may only be declared on a type}}{{5-11=}}

    static let l1: Int = 0 // expected-error {{static properties may only be declared on a type}}{{5-12=}}
    class let l2: Int = 0 // expected-error {{class properties may only be declared on a type}}{{5-11=}}
  }
}

struct S { // expected-note 3{{extended type declared here}}
  static var v1: Int = 0
  class var v2: Int = 0 // expected-error {{class properties are only allowed within classes and protocols; use 'static' to declare a static property}}{{3-8=static}}

  static var v3: Int { return 0 }
  class var v4: Int { return 0 } // expected-error {{class properties are only allowed within classes and protocols; use 'static' to declare a static property}}{{3-8=static}}

  static let l1: Int = 0
  class let l2: Int = 0 // expected-error {{class properties are only allowed within classes and protocols; use 'static' to declare a static property}}{{3-8=static}}
}

extension S {
  static var ev1: Int = 0
  class var ev2: Int = 0 // expected-error {{class properties are only allowed within classes and protocols; use 'static' to declare a static property}}{{3-8=static}}

  static var ev3: Int { return 0 }
  class var ev4: Int { return 0 } // expected-error {{class properties are only allowed within classes and protocols; use 'static' to declare a static property}}{{3-8=static}}

  static let el1: Int = 0
  class let el2: Int = 0 // expected-error {{class properties are only allowed within classes and protocols; use 'static' to declare a static property}}{{3-8=static}}
}

enum E { // expected-note 3{{extended type declared here}}
  static var v1: Int = 0
  class var v2: Int = 0 // expected-error {{class properties are only allowed within classes and protocols; use 'static' to declare a static property}}{{3-8=static}}

  static var v3: Int { return 0 }
  class var v4: Int { return 0 } // expected-error {{class properties are only allowed within classes and protocols; use 'static' to declare a static property}}{{3-8=static}}

  static let l1: Int = 0
  class let l2: Int = 0 // expected-error {{class properties are only allowed within classes and protocols; use 'static' to declare a static property}}{{3-8=static}}
}

extension E {
  static var ev1: Int = 0
  class var ev2: Int = 0 // expected-error {{class properties are only allowed within classes and protocols; use 'static' to declare a static property}}{{3-8=static}}

  static var ev3: Int { return 0 }
  class var ev4: Int { return 0 } // expected-error {{class properties are only allowed within classes and protocols; use 'static' to declare a static property}}{{3-8=static}}

  static let el1: Int = 0
  class let el2: Int = 0 // expected-error {{class properties are only allowed within classes and protocols; use 'static' to declare a static property}}{{3-8=static}}
}

class C { // expected-note 3{{extended type declared here}}
  static var v1: Int = 0 // expected-error {{static properties are only allowed within structs and enums; use 'class' to declare a class property}}{{3-9=class}}
      // expected-error@-1 {{static variables not yet supported in classes}}
  class var v2: Int = 0
      // expected-error@-1 {{static variables not yet supported in classes}}

  static var v3: Int { return 0 } // expected-error {{static properties are only allowed within structs and enums; use 'class' to declare a class property}}{{3-9=class}}
  class var v4: Int { return 0 }

  static let l1: Int = 0 // expected-error {{static properties are only allowed within structs and enums; use 'class' to declare a class property}}{{3-9=class}}
      // expected-error@-1 {{static variables not yet supported in classes}}
  class let l2: Int = 0
      // expected-error@-1 {{static variables not yet supported in classes}}
}

extension C {
  static var ev1: Int = 0 // expected-error {{static properties are only allowed within structs and enums; use 'class' to declare a class property}}{{3-9=class}}
      // expected-error@-1 {{static variables not yet supported in classes}}
  class var ev2: Int = 0
      // expected-error@-1 {{static variables not yet supported in classes}}

  static var ev3: Int { return 0 } // expected-error {{static properties are only allowed within structs and enums; use 'class' to declare a class property}}{{3-9=class}}
  class var ev4: Int { return 0 }

  static let el1: Int = 0 // expected-error {{static properties are only allowed within structs and enums; use 'class' to declare a class property}}{{3-9=class}}
      // expected-error@-1 {{static variables not yet supported in classes}}
  class let el2: Int = 0
      // expected-error@-1 {{static variables not yet supported in classes}}
}

protocol P {
  static var v1: Int // expected-error {{static properties are only allowed within structs and enums; use 'class' to declare a class property}}{{3-9=class}}
      // expected-error@-1 {{static variables not yet supported in protocols}}
  class var v2: Int // expected-error {{static variables not yet supported in protocols}}

  static var v3: Int { get } // expected-error {{static properties are only allowed within structs and enums; use 'class' to declare a class property}}{{3-9=class}}
      // expected-error@-1 {{static variables not yet supported in protocols}}
  class var v4: Int { get }
      // expected-error@-1 {{static variables not yet supported in protocols}}

  static let l1: Int // expected-error {{static properties are only allowed within structs and enums; use 'class' to declare a class property}}{{3-9=class}}
      // expected-error@-1 {{static variables not yet supported in protocols}}
  class let l2: Int // expected-error {{static variables not yet supported in protocols}}
}



struct S1 {
  // rdar://15626843
  static var x: Int  // expected-error {{'static var' declaration require an initializer expression or getter/setter specifier}}

  var y = 1

  static var z = 5
}

extension S1 {
  static var zz = 42
  static var xy: Int { return 5 }
}

enum E1 {
  static var y: Int { get: }
}

class C1 {
  class var x: Int // expected-error {{'class var' declaration require an initializer expression or getter/setter specifier}}
      // expected-error@-1 {{static variables not yet supported in classes}}
}

