// RUN: %swift %s -verify

struct X { }
struct Y { }

def +(lhs: X, rhs: X) -> X { } // okay

def +++(lhs: X, rhs: X) -> X { } // expected-error{{operator implementation without matching operator declaration}}

operator infix ++++ {
  precedence 195
  associativity left
}

def ++++(lhs: X, rhs: X) -> X { }
def ++++(lhs: Y, rhs: Y) -> Y { } // okay

// Assignment operators
operator infix +- {
  precedence 90
  associativity left
}
@assignment def +-(x: Int, y: Int) {} // expected-error{{assignment operator must have an initial @inout argument}}
@assignment def assign(x: Int, y: Int) {} // expected-error{{attribute cannot be applied to declaration}}

def use_assignments(i: Int, j: Int) {
 ++i
 i += j
 ++(&i);
 &i += j
}

def useInt(x: Int) { }

def test() {
  var x : Int
  var y : Int = 42
  // Produce a diagnostic for using the result of an assignment as a value.
  // rdar://12961094
  useInt(x = y)  // expected-error{{expression does not type-check}}
}

operator prefix ~~ {}
operator postfix ~~ {}
operator infix ~~ {}

@postfix def foo(x: Int) {} // expected-error {{only operator functions can be declared 'postfix'}}
@postfix def ~~(x: Int) -> Float { return Float(x) }
@postfix def ~~(x: Int, y: Int) {} // expected-error {{only unary operators can be declared 'postfix'}}
@prefix def ~~(x: Float) {}
def test_postfix(x: Int) {
  ~~x~~
}

operator prefix ~~~ {} // expected-note 2{{prefix operator found here}}

// Unary operators require a prefix or postfix attribute
def ~~~(x: Float) {} // expected-error{{prefix unary operator missing 'prefix' attribute}}{{5-5=@prefix }}

protocol P {
  def ~~~(x: Self) // expected-error{{prefix unary operator missing 'prefix' attribute}}{{7-7=@prefix }}
}

@prefix def +// this should be a comment, not an operator
(arg: Int) -> Int { return arg }

@prefix def -/* this also should be a comment, not an operator */
(arg: Int) -> Int { return arg }

def +*/ () {}   // expected-error {{expected identifier in function declaration}} expected-error {{unexpected end of block comment}} expected-error {{braced block of statements is an unused closure}}

def errors() {
  */    // expected-error {{unexpected end of block comment}}
  
  // rdar://12962712 - reject */ in an operator as it should end a block comment.
  */+    // expected-error {{unexpected end of block comment}}
}

operator prefix .. {}

@prefix def .. (arg: Int) -> Int { return arg }
def ... () { } // expected-error {{expected identifier in function declaration}} expected-error {{braced block of statements is an unused closure}}
def resyncParser() {}
def .... () { } // expected-error {{unexpected long series of '.'}} expected-error {{expected identifier in function declaration}} expected-error {{braced block of statements is an unused closure}}

// Operator decl refs (<op>)

operator infix +-+ {}
operator prefix +-+ {}

operator prefix -+- {}
operator postfix -+- {}

operator infix +-+= {}

@infix def +-+ (x:Int, y:Int) -> Int {}
@prefix def +-+ (x:Int) -> Int {}

@assignment @prefix def -+- (y:@inout Int) -> Int {} // expected-note 2{{found this candidate}}
@assignment @postfix def -+- (x:@inout Int) -> Int {} // expected-note 2{{found this candidate}}

@assignment @infix def +-+= (x:@inout Int, y:Int) -> Int {}

var n = 0

// Infix by context
(+-+)(1, 2)
// Prefix by context
(+-+)(1)

// Ambiguous -- could be prefix or postfix
(-+-)(&n) // expected-error{{ambiguous use of operator '-+-'}}

// Assignment operator refs become inout functions
(+-+=)(&n, 12)
(+-+=)(n, 12) // FIXME: should error

var f1 : (Int, Int) -> Int = (+-+)
var f2 : (Int) -> Int = (+-+)
var f3 : (@inout Int) -> Int = (-+-) // expected-error{{ambiguous use of operator '-+-'}}
var f4 : (@inout Int, Int) -> Int = (+-+=)
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

def ☃(x: Int, y: Int) -> Bool { return x == y }
def ☃⃠(x: Int, y: Int) -> Bool { return x != y }

var x, y : Int
x☃y
x☃⃠y

// rdar://14705150 - crash on invalid
def test_14705150() {
  var a = 4
  var b! = a  // expected-error {{consecutive statements on a line must be separated by ';'}} \
              // expected-error {{expected expression}} \
              // expected-error {{type annotation missing in pattern}}
}

@prefix @postfix def ++(x: Int) {} // expected-error {{attribute 'prefix' cannot be combined with this attribute}}
@postfix @prefix def ++(x: Int) {} // expected-error {{attribute 'postfix' cannot be combined with this attribute}}

// Don't allow one to define a postfix '!'; it's built into the
// language.
operator postfix! {} // expected-error{{cannot declare a custom postfix '!' operator}}
@postfix def !(x: Int) { } // expected-error{{cannot declare a custom postfix '!' operator}}
@postfix def!(x: Int8) { } // expected-error{{cannot declare a custom postfix '!' operator}}
