// RUN: %swift %s -verify

//===----------------------------------------------------------------------===//
// Tests for various simple enum constructs
//===----------------------------------------------------------------------===//


enum unionSearchFlags {
  case None
  case Backwards
  case Anchored

  init() { self = .None }
}

def test1() -> unionSearchFlags {
  var a : unionSearchFlags
  var b = unionSearchFlags.None
  b = unionSearchFlags.Anchored

  return unionSearchFlags.Backwards
}

def test1a() -> unionSearchFlags {
  var a : unionSearchFlags
  var b : unionSearchFlags = .None
  b = .Anchored

  // ForwardIndex use of MaybeInt.
  var c = MaybeInt.None

  return .Backwards
}

def test1b() {
  var x = 123
  .true == true // should NOT parse as "123.true"
}

enum MaybeInt {
  case None
  case Some(Int)
}

def test2(a: Int, b: Int, c: MaybeInt) {
  var c = MaybeInt.Some(4)
  var d = MaybeInt.Some  // Bind the function itself.
  var e = MaybeInt.Some(b)

  test2(1, 2, .None)
}

enum ZeroOneTwoThree {
  case Zero
  case One(Int)
  case Two(Int, Int)
  case Three(Int,Int,Int)
  case Unknown(MaybeInt, MaybeInt, MaybeInt)
}

def test3(a: ZeroOneTwoThree) {
  var e = ZeroOneTwoThree.Three(1,2,3)
  var f = ZeroOneTwoThree.Unknown(MaybeInt.None, MaybeInt.Some(4),
                                  MaybeInt.Some(32))
  var f1 = ZeroOneTwoThree(MaybeInt.None, MaybeInt(4), MaybeInt(32))

  var g : Int =
     ZeroOneTwoThree.Zero // expected-error {{'ZeroOneTwoThree' is not convertible to 'Int'}}

  test3 ZeroOneTwoThree.Zero // expected-error {{expression resolves to an unused function}} expected-error{{consecutive statements}}
  test3 (ZeroOneTwoThree.Zero)
  test3(ZeroOneTwoThree.Zero)
  test3 // expected-error {{expression resolves to an unused function}}
  (ZeroOneTwoThree.Zero)
  
  var h : ZeroOneTwoThree = ((.One))(4)
  
  var inf : (Int,Int) -> ZeroOneTwoThree = .Two
}

def test3a(a: ZeroOneTwoThree) {
  var e : ZeroOneTwoThree = (.Three(1, 2, 3))
  var f = ZeroOneTwoThree.Unknown(.None, .Some(4), .Some(32))

  var g = .None  // expected-error {{expression does not type-check}}

  // Overload resolution can resolve this to the right constructor.
  var h = ZeroOneTwoThree(1)

  test3a;  // expected-error {{unused function}}
  .Zero   // expected-error {{expression does not type-check}}
  test3a   // expected-error {{unused function}}
  (.Zero) // expected-error {{expression does not type-check}}
  test3a(.Zero)
}


struct CGPoint { var x : Int, y : Int }
typealias OtherPoint = ( x : Int, y : Int)

def test4() {
  var a : CGPoint
  // Note: we reject the following because it conflicts with the current
  // "init" hack.
  var b = CGPoint.CGPoint(1, 2) // expected-error {{'CGPoint.metatype' does not have a member named 'CGPoint'}}
  var c = CGPoint(y : 1, x : 2)   // Using injected name.

  var d : Int =
    CGPoint(y : 1, x : 2)   // expected-error {{'CGPoint' is not convertible to 'Int'}}

  var e = CGPoint.x // expected-error {{'CGPoint.metatype' does not have a member named 'x'}}
  var f = OtherPoint.x  // expected-error {{'OtherPoint.metatype' does not have a member named 'x'}}
}



struct CGSize { var width : Int, height : Int }

extension CGSize {
  def area() -> Int {
    return width*self.height
  }
  
  def area_wrapper() -> Int {
    return area()
  }
}

struct CGRect { 
  var origin : CGPoint,
  size : CGSize
  
  def area() -> Int {
    return self.size.area()
  }
}

def area(r: CGRect) -> Int {
  return r.size.area()
}

extension CGRect {
  def search(x: Int) -> CGSize {}
  def bad_search(Int) -> CGSize {} // expected-error {{type annotation missing in pattern}}
}

def test5(myorigin: CGPoint) {
  var x1 = CGRect(myorigin, CGSize(42, 123))
  var x2 = CGRect(size : CGSize(width : 42, height:123), origin : myorigin)

  4+5

  // Dot syntax.
  var x3 = x2.origin.x
  var x4 = x1.size.area()
  var x4a = (r : x1.size).r.area()
  var x5 = x1.size.area()
  var x5a = (r : x1.size).r.area()
  
  var x5b = x1.area //expected-error{{partial application of struct or enum method is not allowed}}

  var x6 = x1.search(42)
  var x7 = x1.search(42).width

  // TODO: something like this (name binding on the LHS):
  // var (CGSize(width, height)) = CGSize(1,2)

  // TODO: something like this, how do we get it in scope in the {} block?
  //if (var Some(x) = somemaybeint) { ... }

  
}

struct StructTest1 {
  var a : Int, c, b : Int


  typealias ElementType = Int
}
def +() {} // expected-error {{operators must have one or two arguments}}

enum UnionTest1 {
  case x
  case y(Int)

  def foo() {}

  init() { self = .x }
}


extension UnionTest1 {
  def foo() {}
  def bar() {}

  // metatype method.
  static def baz() {}
}

struct EmptyStruct {
  def foo() {}
}

static def global_static_func() {  // expected-error {{'static' functions may only be declared on a type}} {{1-8=}}
}

def f() { 
  var a : UnionTest1
  a.bar()
  UnionTest1.baz()  // dot syntax access to a static method.
  
  // Test that we can get the "address of a member".
  var member_ptr_static : () -> () = UnionTest1.baz
  var member_ptr : (@inout UnionTest1) -> () -> () = UnionTest1.bar
}

def union_error(a: ZeroOneTwoThree) {
  var t1 : ZeroOneTwoThree = .Zero(1) // expected-error {{expression does not type-check}}
  var t2 : ZeroOneTwoThree = .One // expected-error {{expression does not type-check}}
  var t3 : ZeroOneTwoThree = .foo // expected-error {{expression does not type-check}}
  var t4 : ZeroOneTwoThree = .foo() // expected-error {{expression does not type-check}}
}

def local_struct() {
  struct s { def y() {} }
}

//===----------------------------------------------------------------------===//
// A silly units example showing "user defined literals".
//===----------------------------------------------------------------------===//

struct distance { var v : Int }

def - (lhs: distance, rhs: distance) -> distance {}

extension Int {
  def km() -> distance {}
  def cm() -> distance {}
}

def units(x: Int) -> distance {
  x.km() - 4.cm() - 42.km()
}



var %% : distance -> distance // expected-error {{expected pattern}}

def badTupleElement() {
  typealias X = (x : Int, y : Int)
  var y = X.y // expected-error{{'X.metatype' does not have a member named 'y'}}
  var z = X.z // expected-error{{'X.metatype' does not have a member named 'z'}}
}
