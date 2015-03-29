// RUN: %target-parse-verify-swift

infix operator %%% {}
infix operator %%%% {}

func %%%() {} // expected-error {{operators must have one or two arguments}}
func %%%%(a: Int, b: Int, c: Int) {} // expected-error {{operators must have one or two arguments}}

struct X {}
struct Y {}

func +(lhs: X, rhs: X) -> X {} // okay

func +++(lhs: X, rhs: X) -> X {} // expected-error {{operator implementation without matching operator declaration}}

infix operator ++++ {
  precedence 195
  associativity left
}

infix func fn_binary(lhs: Int, rhs: Int) {}  // expected-error {{'infix' requires a function with an operator identifier}}


func ++++(lhs: X, rhs: X) -> X {}
func ++++(lhs: Y, rhs: Y) -> Y {} // okay

func useInt(x: Int) {}
func test() {
  var x : Int
  var y : Int = 42
  // Produce a diagnostic for using the result of an assignment as a value.
  // rdar://12961094
  useInt(x = y)  // expected-error{{cannot invoke 'useInt' with an argument list of type '(())'}} expected-note{{expected an argument list of type '(Int)'}}
}

prefix operator ~~ {}
postfix operator ~~ {}
infix operator ~~ {}

postfix func foo(x: Int) {} // expected-error {{'postfix' requires a function with an operator identifier}}
postfix func ~~(x: Int) -> Float { return Float(x) }
postfix func ~~(x: Int, y: Int) {} // expected-error {{'postfix' requires a function with one argument}}
prefix func ~~(x: Float) {}
func test_postfix(x: Int) {
  ~~x~~
}

prefix operator ~~~ {} // expected-note 2{{prefix operator found here}}

// Unary operators require a prefix or postfix attribute
func ~~~(x: Float) {} // expected-error{{prefix unary operator missing 'prefix' modifier}}{{1-1=prefix }}

protocol P {
  func ~~~(x: Self) // expected-error{{prefix unary operator missing 'prefix' modifier}}{{3-3=prefix }}
}

prefix func +// this should be a comment, not an operator
(arg: Int) -> Int { return arg }

prefix func -/* this also should be a comment, not an operator */
(arg: Int) -> Int { return arg }

func +*/ () {}   // expected-error {{expected identifier in function declaration}} expected-error {{unexpected end of block comment}} expected-error {{braced block of statements is an unused closure}} expected-error{{begin with a closure}} expected-note{{discard the result}} expected-error{{type of expression is ambiguous without more context}}
func errors() {
  */    // expected-error {{unexpected end of block comment}}
  
  // rdar://12962712 - reject */ in an operator as it should end a block comment.
  */+    // expected-error {{unexpected end of block comment}}
}

prefix operator ... {}

prefix func ... (arg: Int) -> Int { return arg }
func resyncParser() {}

// Operator decl refs (<op>)

infix operator +-+ {}
prefix operator +-+ {}

prefix operator -+- {}
postfix operator -+- {}

infix operator +-+= {}

infix func +-+ (x: Int, y: Int) -> Int {} // expected-error {{'infix' modifier is not required or allowed on func declarations}}
prefix func +-+ (x: Int) -> Int {}

prefix func -+- (inout y: Int) -> Int {} // expected-note 2{{found this candidate}}
postfix func -+- (inout x: Int) -> Int {} // expected-note 2{{found this candidate}}

infix func +-+= (inout x: Int, y: Int) -> Int {} // expected-error {{'infix' modifier is not required or allowed on func declarations}}

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
infix operator ☃ {}
infix operator ☃⃠ {} // Operators can contain (but not start with) combining characters

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

prefix postfix func ++(x: Int) {} // expected-error {{attribute 'prefix' cannot be combined with this attribute}}
postfix prefix func ++(x: Int) {} // expected-error {{attribute 'postfix' cannot be combined with this attribute}}

// Don't allow one to define a postfix '!'; it's built into the
// language.
postfix operator! {}  // expected-error {{cannot declare a custom postfix '!' operator}}
prefix operator & {}  // expected-error {{cannot declare a custom prefix '&' operator}}

// <rdar://problem/14607026> Restrict use of '<' and '>' as prefix/postfix operator names
postfix operator > {}  // expected-error {{cannot declare a custom postfix '>' operator}}
prefix operator < {}  // expected-error {{cannot declare a custom prefix '<' operator}}

postfix func !(x: Int) { } // expected-error{{cannot declare a custom postfix '!' operator}}
postfix func!(x: Int8) { } // expected-error{{cannot declare a custom postfix '!' operator}}
prefix func & (x: Int) {} // expected-error {{cannot declare a custom prefix '&' operator}}

// Only allow operators at global scope:
func operator_in_func_bad () {
    prefix func + (input: String) -> String { return "+" + input } // expected-error {{operators are only allowed at global scope}} expected-error{{braced block of statements is an unused closure}} \
                                                                    // expected-error {{use of unresolved identifier 'input'}}
}

infix operator ? {}  // expected-error {{expected operator name in operator declaration}} expected-error {{braced block of statements is an unused closure}} expected-error{{begin with a closure}} expected-note{{discard the result}} expected-error{{type of expression is ambiguous without more context}}

infix operator ??= {}

func ??= <T>(inout result : T?, rhs : Int) {  // ok
}


