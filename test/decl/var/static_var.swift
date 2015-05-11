// RUN: %target-parse-verify-swift -parse-as-library

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
    // expected-error@-1 {{global 'let' declaration requires an initializer expression}}
class let glu2: Int // expected-error {{class properties may only be declared on a type}}{{1-6=}}
    // expected-error@-1 {{global 'let' declaration requires an initializer expression}}
override static let glu3: Int // expected-error {{static properties may only be declared on a type}}{{10-16=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{1-9=}}
    // expected-error@-2 {{global 'let' declaration requires an initializer expression}}
override class let glu4: Int // expected-error {{class properties may only be declared on a type}}{{10-15=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{1-9=}}
    // expected-error@-2 {{global 'let' declaration requires an initializer expression}}
static override let glu5: Int // expected-error {{static properties may only be declared on a type}}{{1-7=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{8-16=}}
    // expected-error@-2 {{global 'let' declaration requires an initializer expression}}
class override let glu6: Int // expected-error {{class properties may only be declared on a type}}{{1-6=}}
    // expected-error@-1 {{'override' can only be specified on class members}}{{7-15=}}
    // expected-error@-2 {{global 'let' declaration requires an initializer expression}}


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
  
  v1 = 1; v2 = 1
  _ = v1+v2+l1+l2
}

struct InMemberFunc {
  func member() {
    static var v1: Int // expected-error {{static properties may only be declared on a type}}{{5-11=}}
    class var v2: Int // expected-error {{class properties may only be declared on a type}}{{5-10=}}

    static let l1: Int = 0 // expected-error {{static properties may only be declared on a type}}{{5-11=}}
    class let l2: Int = 0 // expected-error {{class properties may only be declared on a type}}{{5-10=}}

    v1 = 1; v2 = 1
    _ = v1+v2+l1+l2
  }
}

struct S { // expected-note 3{{extended type declared here}}
  static var v1: Int = 0
  class var v2: Int = 0 // expected-error {{class properties are only allowed within classes; use 'static' to declare a static property}}

  static var v3: Int { return 0 }
  class var v4: Int { return 0 } // expected-error {{class properties are only allowed within classes; use 'static' to declare a static property}}
  static final var v5 = 1 // expected-error {{only classes and class members may be marked with 'final'}}

  static let l1: Int = 0
  class let l2: Int = 0 // expected-error {{class properties are only allowed within classes; use 'static' to declare a static property}}
  static final let l3 = 1 // expected-error {{only classes and class members may be marked with 'final'}}
}

extension S {
  static var ev1: Int = 0
  class var ev2: Int = 0 // expected-error {{class properties are only allowed within classes; use 'static' to declare a static property}}

  static var ev3: Int { return 0 }
  class var ev4: Int { return 0 } // expected-error {{class properties are only allowed within classes; use 'static' to declare a static property}}

  static let el1: Int = 0
  class let el2: Int = 0 // expected-error {{class properties are only allowed within classes; use 'static' to declare a static property}}
}

enum E { // expected-note 3{{extended type declared here}}
  static var v1: Int = 0
  class var v2: Int = 0 // expected-error {{class properties are only allowed within classes; use 'static' to declare a static property}}

  static var v3: Int { return 0 }
  class var v4: Int { return 0 } // expected-error {{class properties are only allowed within classes; use 'static' to declare a static property}}

  static final var v5 = 1 // expected-error {{only classes and class members may be marked with 'final'}}

  static let l1: Int = 0
  class let l2: Int = 0 // expected-error {{class properties are only allowed within classes; use 'static' to declare a static property}}
  static final let l3 = 1 // expected-error {{only classes and class members may be marked with 'final'}}
}

extension E {
  static var ev1: Int = 0
  class var ev2: Int = 0 // expected-error {{class properties are only allowed within classes; use 'static' to declare a static property}}

  static var ev3: Int { return 0 }
  class var ev4: Int { return 0 } // expected-error {{class properties are only allowed within classes; use 'static' to declare a static property}}

  static let el1: Int = 0
  class let el2: Int = 0 // expected-error {{class properties are only allowed within classes; use 'static' to declare a static property}}
}

class C {
  static var v1: Int = 0
  class final var v3: Int = 0 // expected-error {{class stored properties not yet supported}}
  class var v4: Int = 0 // expected-error {{class stored properties not yet supported}}

  static var v5: Int { return 0 }
  class var v6: Int { return 0 }
  static final var v7: Int = 0 // expected-error {{static declarations are already final}}

  static let l1: Int = 0
  class let l2: Int = 0 // expected-error {{class stored properties not yet supported in classes; did you mean 'static'?}}
  class final let l3: Int = 0 // expected-error {{class stored properties not yet supported}}
  static final let l4 = 2 // expected-error {{static declarations are already final}}
}

extension C {
  static var ev1: Int = 0
  class final var ev2: Int = 0 // expected-error {{class stored properties not yet supported}}
  class var ev3: Int = 0 // expected-error {{class stored properties not yet supported}}

  static var ev4: Int { return 0 }
  class var ev5: Int { return 0 }
  static final var ev6: Int = 0 // expected-error {{static declarations are already final}}

  static let el1: Int = 0
  class let el2: Int = 0 // expected-error {{class stored properties not yet supported in classes; did you mean 'static'?}}
  class final let el3: Int = 0 // expected-error {{class stored properties not yet supported in classes; did you mean 'static'?}}
  static final let el4: Int = 0 // expected-error {{static declarations are already final}}
}

protocol P {
  // Both `static` and `class` property requirements are equivalent in protocols rdar://problem/17198298
  static var v1: Int { get }
  class var v2: Int { get } // expected-error {{class properties are only allowed within classes; use 'static' to declare a static property}}
  static final var v3: Int { get } // expected-error {{only classes and class members may be marked with 'final'}}

  static let l1: Int // expected-error {{static stored properties not yet supported in generic types}} expected-error {{immutable property requirement must be declared as 'var' with a '{ get }' specifier}}
  class let l2: Int // expected-error {{class properties are only allowed within classes; use 'static' to declare a static property}} expected-error {{class stored properties not yet supported in generic types}} expected-error {{immutable property requirement must be declared as 'var' with a '{ get }' specifier}}
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
  class var x: Int // expected-error {{class stored properties not yet supported}} expected-error {{'class var' declaration requires an initializer expression or getter/setter specifier}}
}

class C2 {
  var x: Int = 19
  class var x: Int = 17 // expected-error{{class stored properties not yet supported}}

  func xx() -> Int { return self.x + C2.x }
}

class ClassHasVars {
  static var computedStatic: Int { return 0 } // expected-note {{overridden declaration is here}}
  class var computedClass: Int { return 0 }
  var computedInstance: Int { return 0 }
}

class ClassOverridesVars : ClassHasVars {
  override static var computedStatic: Int { return 1 } // expected-error {{class var overrides a 'final' class var}}
  override class var computedClass: Int { return 1 }
  override var computedInstance: Int { return 1 }
}

struct S2 {
  var x: Int = 19
  static var x: Int = 17

  func xx() -> Int { return self.x + C2.x }
}

// rdar://problem/19887250
protocol Proto {
  static var name: String {get set}
}
struct ProtoAdopter : Proto {
  static var name: String = "name" // no error, even though static setters aren't mutating
}


// rdar://18990358
public struct Foo {
  public static let S { a // expected-error{{computed property must have an explicit type}}
    // expected-error@-1{{type annotation missing in pattern}}
    // expected-error@-2{{'let' declarations cannot be computed properties}}
    // expected-error@-3{{use of unresolved identifier 'a'}}
}

// expected-error@+1 {{expected declaration}}
