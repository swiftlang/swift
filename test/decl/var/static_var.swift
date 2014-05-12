// RUN: %swift -parse -parse-as-library %s -verify

// See also rdar://15626843.
static var gvu1: Int // expected-error {{static properties may only be declared on a type}}{{1-7=}}
    // expected-error@-1 {{global 'var' declaration requires an initializer expression or getter/setter specifier}}
class var gvu2: Int // expected-error {{class properties may only be declared on a type}}{{1-6=}}
    // expected-error@-1 {{global 'var' declaration requires an initializer expression or getter/setter specifier}}
override static var gvu3: Int // expected-error {{static properties may only be declared on a type}}{{10-16=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{1-9=}}
    // expected-error@-2 {{global 'var' declaration requires an initializer expression or getter/setter specifier}}
override class var gvu4: Int // expected-error {{class properties may only be declared on a type}}{{10-15=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{1-9=}}
    // expected-error@-2 {{global 'var' declaration requires an initializer expression or getter/setter specifier}}
static override var gvu5: Int // expected-error {{static properties may only be declared on a type}}{{1-7=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{8-16=}}
    // expected-error@-2 {{global 'var' declaration requires an initializer expression or getter/setter specifier}}
class override var gvu6: Int // expected-error {{class properties may only be declared on a type}}{{1-6=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{7-15=}}
    // expected-error@-2 {{global 'var' declaration requires an initializer expression or getter/setter specifier}}

static var gvu7: Int { // expected-error {{static properties may only be declared on a type}}{{1-7=}}
  return 42
}
class var gvu8: Int { // expected-error {{class properties may only be declared on a type}}{{1-6=}}
  return 42
}


static let glu1: Int // expected-error {{static properties may only be declared on a type}}{{1-7=}}
    // expected-error@-1 {{'let' declarations require an initializer expression}}
class let glu2: Int // expected-error {{class properties may only be declared on a type}}{{1-6=}}
    // expected-error@-1 {{'let' declarations require an initializer expression}}
override static let glu3: Int // expected-error {{static properties may only be declared on a type}}{{10-16=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{1-9=}}
    // expected-error@-2 {{'let' declarations require an initializer expression}}
override class let glu4: Int // expected-error {{class properties may only be declared on a type}}{{10-15=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{1-9=}}
    // expected-error@-2 {{'let' declarations require an initializer expression}}
static override let glu5: Int // expected-error {{static properties may only be declared on a type}}{{1-7=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{8-16=}}
    // expected-error@-2 {{'let' declarations require an initializer expression}}
class override let glu6: Int // expected-error {{class properties may only be declared on a type}}{{1-6=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{7-15=}}
    // expected-error@-2 {{'let' declarations require an initializer expression}}


static var gvi1: Int = 0 // expected-error {{static properties may only be declared on a type}}{{1-7=}}
class var gvi2: Int = 0 // expected-error {{class properties may only be declared on a type}}{{1-6=}}
override static var gvi3: Int = 0 // expected-error {{static properties may only be declared on a type}}{{10-16=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{1-9=}}
override class var gvi4: Int = 0 // expected-error {{class properties may only be declared on a type}}{{10-15=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{1-9=}}
static override var gvi5: Int = 0 // expected-error {{static properties may only be declared on a type}}{{1-7=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{8-16=}}
class override var gvi6: Int = 0 // expected-error {{class properties may only be declared on a type}}{{1-6=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{7-15=}}


static let gli1: Int = 0 // expected-error {{static properties may only be declared on a type}}{{1-7=}}
class let gli2: Int = 0 // expected-error {{class properties may only be declared on a type}}{{1-6=}}
override static let gli3: Int = 0 // expected-error {{static properties may only be declared on a type}}{{10-16=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{1-9=}}
override class let gli4: Int = 0 // expected-error {{class properties may only be declared on a type}}{{10-15=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{1-9=}}
static override let gli5: Int = 0 // expected-error {{static properties may only be declared on a type}}{{1-7=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{8-16=}}
class override let gli6: Int = 0 // expected-error {{class properties may only be declared on a type}}{{1-6=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{7-15=}}


func inGlobalFunc() {
  static var v1: Int // expected-error {{static properties may only be declared on a type}}{{3-9=}}
  class var v2: Int // expected-error {{class properties may only be declared on a type}}{{3-8=}}

  static let l1: Int = 0 // expected-error {{static properties may only be declared on a type}}{{3-9=}}
  class let l2: Int = 0 // expected-error {{class properties may only be declared on a type}}{{3-8=}}
}

struct InMemberFunc {
  func member() {
    static var v1: Int // expected-error {{static properties may only be declared on a type}}{{5-11=}}
    class var v2: Int // expected-error {{class properties may only be declared on a type}}{{5-10=}}

    static let l1: Int = 0 // expected-error {{static properties may only be declared on a type}}{{5-11=}}
    class let l2: Int = 0 // expected-error {{class properties may only be declared on a type}}{{5-10=}}
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
      // expected-error@-1 {{static variables not yet supported}}
  class var v2: Int = 0
      // expected-error@-1 {{class variables not yet supported}}

  static var v3: Int { return 0 } // expected-error {{static properties are only allowed within structs and enums; use 'class' to declare a class property}}{{3-9=class}}
  class var v4: Int { return 0 }

  static let l1: Int = 0 // expected-error {{static properties are only allowed within structs and enums; use 'class' to declare a class property}}{{3-9=class}}
      // expected-error@-1 {{static variables not yet supported}}
  class let l2: Int = 0
      // expected-error@-1 {{class variables not yet supported}}
}

extension C {
  static var ev1: Int = 0 // expected-error {{static properties are only allowed within structs and enums; use 'class' to declare a class property}}{{3-9=class}}
      // expected-error@-1 {{static variables not yet supported}}
  class var ev2: Int = 0
      // expected-error@-1 {{class variables not yet supported}}

  static var ev3: Int { return 0 } // expected-error {{static properties are only allowed within structs and enums; use 'class' to declare a class property}}{{3-9=class}}
  class var ev4: Int { return 0 }

  static let el1: Int = 0 // expected-error {{static properties are only allowed within structs and enums; use 'class' to declare a class property}}{{3-9=class}}
      // expected-error@-1 {{static variables not yet supported}}
  class let el2: Int = 0
      // expected-error@-1 {{class variables not yet supported}}
}

protocol P {
  static var v1: Int { get } // expected-error {{static properties are only allowed within structs and enums; use 'class' to declare a class property}}{{3-9=class}}
      // expected-error@-1 {{static variables not yet supported in protocols}}
  class var v2: Int { get } // expected-error {{class variables not yet supported in protocols}}

  static var v3: Int { get } // expected-error {{static properties are only allowed within structs and enums; use 'class' to declare a class property}}{{3-9=class}}
      // expected-error@-1 {{static variables not yet supported in protocols}}
  class var v4: Int { get }
      // expected-error@-1 {{class variables not yet supported in protocols}}

  static let l1: Int // expected-error {{static properties are only allowed within structs and enums; use 'class' to declare a class property}}{{3-9=class}}
      // expected-error@-1 {{static variables not yet supported in protocols}}
  class let l2: Int // expected-error {{class variables not yet supported in protocols}}
}



struct S1 {
  // rdar://15626843
  static var x: Int  // expected-error {{'static var' declaration requires an initializer expression or getter/setter specifier}}

  var y = 1

  static var z = 5
}

extension S1 {
  static var zz = 42
  static var xy: Int { return 5 }
}

enum E1 {
  static var y: Int {
    get {}
  }
}

class C1 {
  class var x: Int // expected-error {{'class var' declaration requires an initializer expression or getter/setter specifier}}
      // expected-error@-1 {{class variables not yet supported}}
}

