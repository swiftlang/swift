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

infix func fn_binary(_ lhs: Int, rhs: Int) {}  // expected-error {{'infix' requires a function with an operator identifier}}


func ++++(lhs: X, rhs: X) -> X {}
func ++++(lhs: Y, rhs: Y) -> Y {} // okay

func useInt(_ x: Int) {}
func test() {
  var x : Int  
  let y : Int = 42
  // Produce a diagnostic for using the result of an assignment as a value.
  // rdar://12961094
  useInt(x = y)  // expected-error{{cannot convert value of type '()' to expected argument type 'Int'}}
  _ = x
}

prefix operator ~~ {}
postfix operator ~~ {}
infix operator ~~ {}

postfix func foo(_ x: Int) {} // expected-error {{'postfix' requires a function with an operator identifier}}
postfix func ~~(x: Int) -> Float { return Float(x) }
postfix func ~~(x: Int, y: Int) {} // expected-error {{'postfix' requires a function with one argument}}
prefix func ~~(x: Float) {}
func test_postfix(_ x: Int) {
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

func +*/ () {}   // expected-error {{expected identifier in function declaration}} expected-error {{unexpected end of block comment}} expected-error {{braced block of statements is an unused closure}} expected-error{{begin with a closure}} expected-note{{discard the result}} {{13-13=_ = }} expected-error{{expression resolves to an unused function}}
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

infix func +-+ (x: Int, y: Int) -> Int {} // expected-error {{'infix' modifier is not required or allowed on func declarations}} {{1-7=}}
prefix func +-+ (x: Int) -> Int {}

prefix func -+- (y: inout Int) -> Int {} // expected-note 2{{found this candidate}}
postfix func -+- (x: inout Int) -> Int {} // expected-note 2{{found this candidate}}

infix func +-+= (x: inout Int, y: Int) -> Int {} // expected-error {{'infix' modifier is not required or allowed on func declarations}} {{1-7=}}

var n = 0

// Infix by context
_ = (+-+)(1, 2)
// Prefix by context
_ = (+-+)(1)

// Ambiguous -- could be prefix or postfix
(-+-)(&n) // expected-error{{ambiguous use of operator '-+-'}}

// Assignment operator refs become inout functions
_ = (+-+=)(&n, 12)
(+-+=)(n, 12)   // expected-error {{passing value of type 'Int' to an inout parameter requires explicit '&'}} {{8-8=&}}

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
_ = x☃y
_ = x☃⃠y

// rdar://14705150 - crash on invalid
func test_14705150() {
  let a = 4
  var b! = a  // expected-error {{type annotation missing in pattern}}
  // expected-error @-1 {{consecutive statements on a line must be separated by ';'}} {{8-8=;}}
  // expected-error @-2 {{expected expression}}

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

infix operator ? {}  // expected-error {{expected operator name in operator declaration}} 

infix operator ??= {}

func ??= <T>(result : inout T?, rhs : Int) {  // ok
}



// <rdar://problem/14296004> [QoI] Poor diagnostic/recovery when two operators (e.g., == and -) are adjacted without spaces.
_ = n*-4       // expected-error {{missing whitespace between '*' and '-' operators}} {{6-6= }} {{7-7= }}
if n==-1 {}    // expected-error {{missing whitespace between '==' and '-' operators}} {{5-5= }} {{7-7= }}

prefix operator ☃⃠ {}
prefix func☃⃠(a : Int) -> Int { return a }
postfix operator ☃⃠ {}
postfix func☃⃠(a : Int) -> Int { return a }

_ = n☃⃠ ☃⃠ n   // Ok.
_ = n ☃⃠ ☃⃠n   // Ok.
_ = n ☃⃠☃⃠ n   // expected-error {{use of unresolved operator '☃⃠☃⃠'}}
_ = n☃⃠☃⃠n     // expected-error {{ambiguous missing whitespace between unary and binary operators}}
// expected-note @-1 {{could be binary '☃⃠' and prefix '☃⃠'}} {{12-12= }} {{18-18= }}
// expected-note @-2 {{could be postfix '☃⃠' and binary '☃⃠'}} {{6-6= }} {{12-12= }}

_ = n☃⃠☃⃠ // expected-error {{unary operators may not be juxtaposed; parenthesize inner expression}}
_ = ~!n  // expected-error {{unary operators may not be juxtaposed; parenthesize inner expression}}
_ = -+n  // expected-error {{unary operators may not be juxtaposed; parenthesize inner expression}}
_ = -++n // expected-error {{unary operators may not be juxtaposed; parenthesize inner expression}}

// <rdar://problem/16230507> Cannot use a negative constant as the second operator of ... operator
_ = 3...-5  // expected-error {{missing whitespace between '...' and '-' operators}}

