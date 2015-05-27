// RUN: %target-parse-verify-swift

var t1 : Int
var t2 = 10
var t3 = 10, t4 = 20.0
var (t5, t6) = (10, 20.0)
var t7, t8 : Int
var t9, t10 = 20 // expected-error {{type annotation missing in pattern}}
var t11, t12 : Int = 20 // expected-error {{type annotation missing in pattern}}
var t13 = 2.0, t14 : Int
var (x = 123, // expected-error 2 {{expected ',' separator}} expected-error {{expected pattern}}
     y = 456) : (Int,Int) // expected-error 2 {{expected ',' separator}} expected-error {{expected pattern}}
var bfx : Int, bfy : Int

_ = 10

func _(x: Int) {} // expected-error {{expected identifier in function declaration}}


var self1 = self1 // expected-error {{variable used within its own initial value}}
var self2 : Int = self2 // expected-error {{variable used within its own initial value}}
var (self3) : Int = self3 // expected-error {{variable used within its own initial value}}
var (self4) : Int = self4 // expected-error {{variable used within its own initial value}}
var self5 = self5 + self5 // expected-error 2 {{variable used within its own initial value}}
var self6 = !self6 // expected-error {{variable used within its own initial value}}
var (self7a, self7b) = (self7b, self7a) // expected-error 2 {{variable used within its own initial value}}

var self8 = 0
func testShadowing() {
  var self8 = self8 // expected-error {{variable used within its own initial value}}
}

var (paren) = 0
var paren2: Int = paren

struct Broken {
  var b : Bool = True // expected-error{{use of unresolved identifier 'True'}}
}

// rdar://16252090 - Warning when inferring empty tuple type for declarations
var emptyTuple = testShadowing()  // expected-warning {{variable 'emptyTuple' inferred to have type '()'}} \
                                  // expected-note {{add an explicit type annotation to silence this warning}}

// rdar://15263687 - Diagnose variables inferenced to 'AnyObject'
var ao1 : AnyObject
var ao2 = ao1

var aot1 : AnyObject.Type
var aot2 = aot1          // expected-warning {{variable 'aot2' inferred to have type 'AnyObject.Type', which may be unexpected}} \
                       // expected-note {{add an explicit type annotation to silence this warning}}


for item in [AnyObject]() {  // No warning in for-each loop.
}


// <rdar://problem/16574105> Type inference of _Nil very coherent but kind of useless
var ptr = nil // expected-error {{expression does not conform to type 'NilLiteralConvertible'}}

func testAnyObjectOptional() -> AnyObject? {
  let x = testAnyObjectOptional()
  return x
}

class SomeClass {}

// <rdar://problem/16877304> weak let's should be rejected
weak let V = SomeClass()  // expected-error {{'weak' must be a mutable variable, because it may change at runtime}}

let a = b ; let b = a // expected-error{{could not infer type for 'a'}} 


// <rdar://problem/17501765> Swift should warn about immutable default initialized values
let uselessValue : String?


func tuplePatternDestructuring(x : Int, y : Int) {
  let (b: _, a: h) = (b: x, a: y)
  _ = h
  
  // <rdar://problem/20392122> Destructuring tuple with labels doesn't work
  let (i, j) = (b: x, a: y)
  _ = i+j

  // FIXME: This diagnostic isn't right: rdar://20395243
  let (x: g1, a: h1) = (b: x, a: y)  // expected-error {{'(b: Int, a: Int)' is not convertible to '(x: (b: Int, a: Int), a: (b: Int, a: Int))'}}
}

// <rdar://problem/21057425> Crash while compiling attached test-app.
func test21057425() {
  var x: Int = "not an int!", y = 0 // expected-error{{'String' is not convertible to 'Int'}}
}
