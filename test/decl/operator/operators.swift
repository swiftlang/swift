// RUN: %swift %s -verify

operator infix %%% {}
operator infix %%%% {}

func %%%() {} // expected-error {{operators must have one or two arguments}}
func %%%%(a: Int, b: Int, c: Int) {} // expected-error {{operators must have one or two arguments}}

struct X {}
struct Y {}

func +(lhs: X, rhs: X) -> X {} // okay

func +++(lhs: X, rhs: X) -> X {} // expected-error {{operator implementation without matching operator declaration}}

operator infix ++++ {
  precedence 195
  associativity left
}

func ++++(lhs: X, rhs: X) -> X {}
func ++++(lhs: Y, rhs: Y) -> Y {} // okay

func useInt(x: Int) {} // expected-note{{initialization of parameter}}
func test() {
  var x : Int
  var y : Int = 42
  // Produce a diagnostic for using the result of an assignment as a value.
  // rdar://12961094
  useInt(x = y)  // expected-error{{'()' is not convertible to 'Int'}}
}

operator prefix ~~ {}
operator postfix ~~ {}
operator infix ~~ {}

@postfix func foo(x: Int) {} // expected-error {{only operator functions can be declared 'postfix'}}
@postfix func ~~(x: Int) -> Float { return Float(x) }
@postfix func ~~(x: Int, y: Int) {} // expected-error {{only unary operators can be declared 'postfix'}}
@prefix func ~~(x: Float) {}
func test_postfix(x: Int) {
  ~~x~~
}

operator prefix ~~~ {} // expected-note 2{{prefix operator found here}}

// Unary operators require a prefix or postfix attribute
func ~~~(x: Float) {} // expected-error{{prefix unary operator missing 'prefix' attribute}}{{1-1=@prefix }}

protocol P {
  func ~~~(x: Self) // expected-error{{prefix unary operator missing 'prefix' attribute}}{{3-3=@prefix }}
}

@prefix func +// this should be a comment, not an operator
(arg: Int) -> Int { return arg }

@prefix func -/* this also should be a comment, not an operator */
(arg: Int) -> Int { return arg }

func +*/ () {}   // expected-error {{expected identifier in function declaration}} expected-error {{unexpected end of block comment}} expected-error {{braced block of statements is an unused closure}}
func errors() {
  */    // expected-error {{unexpected end of block comment}}
  
  // rdar://12962712 - reject */ in an operator as it should end a block comment.
  */+    // expected-error {{unexpected end of block comment}}
}

operator prefix ... {}

@prefix func ... (arg: Int) -> Int { return arg }
func resyncParser() {}

// Operator decl refs (<op>)

operator infix +-+ {}
operator prefix +-+ {}

operator prefix -+- {}
operator postfix -+- {}

operator infix +-+= {}

@infix func +-+ (x: Int, y: Int) -> Int {}
@prefix func +-+ (x: Int) -> Int {}

@assignment @prefix func -+- (inout y: Int) -> Int {} // expected-note 2{{found this candidate}}
@assignment @postfix func -+- (inout x: Int) -> Int {} // expected-note 2{{found this candidate}}

@assignment @infix func +-+= (inout x: Int, y: Int) -> Int {}

var n = 0

// Infix by context
(+-+)(1, 2)
// Prefix by context
(+-+)(1)

// Ambiguous -- could be prefix or postfix
(-+-)(&n) // expected-error{{ambiguous use of operator '-+-'}}

// Assignment operator refs become inout functions
(+-+=)(&n, 12)
(+-+=)(n, 12)   // expected-error {{passing value of type 'Int' to an inout parameter requires explicit '&'}}

var f1 : (Int, Int) -> Int = (+-+)
var f2 : (Int) -> Int = (+-+)
var f3 : (inout Int) -> Int = (-+-) // expected-error{{ambiguous use of operator '-+-'}}
var f4 : (inout Int, Int) -> Int = (+-+=)
var r5 : (a : (Int, Int) -> Int, b : (Int, Int) -> Int) = (+, -)
var r6 : (a : (Int, Int) -> Int, b : (Int, Int) -> Int) = (b : +, a : -)

struct f6_S {
  subscript(op : (Int, Int) -> Int) -> Int {
    return 42
  }
}
var f6_s : f6_S
var junk = f6_s[+]

// Unicode operator names
operator infix ☃ {}
operator infix ☃⃠ {} // Operators can contain (but not start with) combining characters

func ☃(x: Int, y: Int) -> Bool { return x == y }
func ☃⃠(x: Int, y: Int) -> Bool { return x != y }

var x, y : Int
x☃y
x☃⃠y

// rdar://14705150 - crash on invalid
func test_14705150() {
  var a = 4
  var b! = a  // expected-error {{consecutive statements on a line must be separated by ';'}} \
              // expected-error {{expected expression}} \
              // expected-error {{type annotation missing in pattern}}
}

@prefix @postfix func ++(x: Int) {} // expected-error {{attribute 'prefix' cannot be combined with this attribute}}
@postfix @prefix func ++(x: Int) {} // expected-error {{attribute 'postfix' cannot be combined with this attribute}}

// Don't allow one to define a postfix '!'; it's built into the
// language.
operator postfix! {} // expected-error{{cannot declare a custom postfix '!' operator}}
@postfix func !(x: Int) { } // expected-error{{cannot declare a custom postfix '!' operator}}
@postfix func!(x: Int8) { } // expected-error{{cannot declare a custom postfix '!' operator}}
